port module Main exposing (main)

import Json.Decode as JD
import Json.Encode as JE
import Connection exposing (..)
import Models.User exposing (..)
import Types exposing (..)
import Http
import Task exposing (Task)
import Dict
import Time


port elmToJs : JsInterface -> Cmd msg


port jsToElm : (JsInterface -> msg) -> Sub msg


decodeDataFromJs : JsInterface -> InboundPortData
decodeDataFromJs jsData =
    let
        decoder =
            case jsData.tag of
                "NewConnection" ->
                    JD.map NewConnection (decodeConnection jsData.connectionId)

                "JsActionResult" ->
                    JD.map2 JsActionResult
                        (JD.succeed jsData.connectionId)
                        JD.value

                "JsError" ->
                    JD.map InboundPortError JD.string

                _ ->
                    JD.fail "Unknown tag, JS -> Elm"
    in
        case JD.decodeValue decoder jsData.payload of
            Ok x ->
                x

            Err str ->
                InboundPortError str


jsActionCmd : OutboundPortAction -> Connection -> Cmd Msg
jsActionCmd elmData conn =
    elmToJs
        { connectionId = conn.id
        , tag = toString elmData |> String.words |> List.head |> Maybe.withDefault ""
        , payload =
            case elmData of
                RespondToClient ->
                    encodeResponse conn.response

                HashPassword plain ->
                    JE.string plain
        }


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        (jsToElm (ReceiveFromJs << decodeDataFromJs))
            :: (if Dict.isEmpty model.pending then
                    []
                else
                    [ Time.every Time.second CollectGarbage ]
               )


init : ( Model, Cmd Msg )
init =
    ( { pending = Dict.empty }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveFromJs (NewConnection conn) ->
            let
                ( maybePending, cmd ) =
                    registerUserStep1 conn
            in
                case maybePending of
                    Just ( connection, continuation ) ->
                        ( { model
                            | pending =
                                Dict.insert
                                    connection.id
                                    ( connection, continuation )
                                    model.pending
                          }
                        , cmd
                        )

                    Nothing ->
                        ( model, cmd )

        ReceiveFromJs (JsActionResult connId jsValue) ->
            case Dict.get connId model.pending of
                Just ( connection, continueWith ) ->
                    ( { model | pending = Dict.remove connId model.pending }
                    , continueWith connection jsValue
                    )

                Nothing ->
                    Debug.log ("Pending connection not found:\n" ++ toString connId)
                        ( model, Cmd.none )

        ReceiveFromJs (InboundPortError str) ->
            ( model, Cmd.none )

        SendToJs RespondToClient ->
            ( model, Cmd.none )

        SendToJs (HashPassword password) ->
            ( model, Cmd.none )

        CollectGarbage now ->
            let
                predicate ( timestamp, seq ) val =
                    now - timestamp > Time.second

                ( keep, dump ) =
                    Dict.partition predicate model.pending

                _ =
                    -- Need to keep whole connection in the dict, and reply properly
                    -- foldl it into a list of commands
                    if not (Dict.isEmpty dump) then
                        Debug.log "Collecting garbage. Dumping..."
                            dump
                    else
                        dump
            in
                ( { pending = keep }, Cmd.none )

        ExecuteCmd cmd ->
            ( model, cmd )


type alias RegistrationFormDetails =
    { username : Username
    , email : Email
    , password : String
    }


type alias RegistrationForm =
    { user : RegistrationFormDetails }


decodeCreateUserInputData : JD.Decoder RegistrationForm
decodeCreateUserInputData =
    JD.map RegistrationForm <|
        JD.field "user" <|
            JD.map3
                RegistrationFormDetails
                (JD.field "username" decodeUsername)
                (JD.field "email" decodeEmail)
                (JD.field "password" JD.string)


createUser : RegistrationFormDetails -> HashAndSalt -> User
createUser formDetails hashAndSalt =
    { username = formDetails.username
    , email = formDetails.email
    , bio = ""
    , image = ""
    , hash = hashAndSalt.hash
    , salt = hashAndSalt.salt
    }



-- toDatabaseJSON


type alias DbCreateDocResponse =
    { id : String
    , ok : Bool
    , rev : String
    }


type DbError
    = BadRequest


type alias DbErrorResponse =
    { error : DbError
    , reason : String
    }


dbUrl : String
dbUrl =
    "http://127.0.0.1:5984/conduit"


createDbDoc : JE.Value -> Http.Request DbCreateDocResponse
createDbDoc json =
    let
        body =
            Http.jsonBody json

        responseDecoder =
            JD.map3
                DbCreateDocResponse
                (JD.field "id" JD.string)
                (JD.field "ok" JD.bool)
                (JD.field "rev" JD.string)
    in
        Http.post dbUrl body responseDecoder


registerUserStep1 : Connection -> ( Maybe ( Connection, Continuation ), Cmd Msg )
registerUserStep1 conn =
    let
        inputResult =
            Debug.log "step 1, decoding input JSON" <|
                JD.decodeString
                    decodeCreateUserInputData
                    conn.request.body
    in
        case inputResult of
            Err str ->
                ( Nothing
                , jsActionCmd RespondToClient
                    { conn
                        | response =
                            Connection.errorResponse
                                Connection.BadRequest
                                conn.response
                    }
                )

            Ok regFormData ->
                ( Just
                    ( conn
                    , registerUserStep2 regFormData
                    )
                , jsActionCmd (HashPassword regFormData.user.password) conn
                )


type alias HashAndSalt =
    { hash : String
    , salt : String
    }


decodeHashAndSalt : JD.Decoder HashAndSalt
decodeHashAndSalt =
    JD.map2 HashAndSalt
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


registerUserStep2 : RegistrationForm -> Connection -> JD.Value -> Cmd Msg
registerUserStep2 regFormData conn jsHashAndSalt =
    case JD.decodeValue decodeHashAndSalt jsHashAndSalt of
        Ok hashAndSalt ->
            registerUserStep3 regFormData conn hashAndSalt

        Err e ->
            jsActionCmd RespondToClient
                { conn
                    | response = errorResponse InternalError conn.response
                }


registerUserStep3 : RegistrationForm -> Connection -> HashAndSalt -> Cmd Msg
registerUserStep3 regFormData conn hashAndSalt =
    let
        user =
            Debug.log "step 3, creating user datastructure" <|
                createUser
                    regFormData.user
                    (Debug.log "step 3, receiving hash & salt"
                        hashAndSalt
                    )

        dbTask =
            user
                |> toDatabaseJSON
                |> createDbDoc
                |> Http.toTask

        successBody =
            JE.encode 0 <| toAuthJSON user

        handleDbResult : Result Http.Error DbCreateDocResponse -> Msg
        handleDbResult dbResult =
            ExecuteCmd <|
                jsActionCmd RespondToClient <|
                    { conn
                        | response =
                            case dbResult of
                                Ok _ ->
                                    successResponse
                                        (Debug.log "DB doc created, creating response"
                                            successBody
                                        )
                                        conn.response

                                Err httpError ->
                                    handleDbError conn.response
                                        (Debug.log "DB doc creation failed, creating error response"
                                            httpError
                                        )
                    }
    in
        Task.attempt handleDbResult dbTask


handleDbError : Response -> Http.Error -> Response
handleDbError response httpError =
    case httpError of
        Http.BadUrl url ->
            errorResponse Connection.NotFound response

        Http.Timeout ->
            errorResponse Connection.RequestTimeout response

        Http.NetworkError ->
            errorResponse Connection.RequestTimeout response

        Http.BadStatus httpResponse ->
            { response
                | statusCode = httpResponse.status.code
                , body =
                    -- Debug.log "DB BadStatus"
                    httpResponse.body
            }

        Http.BadPayload jsonDecoderString httpResponse ->
            { response
                | statusCode = 500
                , body =
                    -- Debug.log ("DB BadPayload\n" ++ jsonDecoderString ++ "\n")
                    httpResponse.body
            }



--
{-



   - assemble DB JSON (pure)
   - send JSON to DB (HTTP effect, task)
   - receive DB response (task andThen)
   - handle errors
   - construct response object
   - send command to port


   router.post('/users', function(req, res, next) {
   var user = new User();

   user.username = req.body.user.username;
   user.email = req.body.user.email;
   user.setPassword(req.body.user.password);

   user
       .save()
       .then(function() {
       return res.json({ user: user.toAuthJSON() });
       })
       .catch(next);
   });

-}
