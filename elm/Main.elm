module Main exposing (main)

import Json.Decode as JD
import Json.Encode as JE
import Connection exposing (decodeConnection, encodeResponse, successResponse, errorResponse)
import Routes exposing (generateResponse)
import Models.User exposing (..)
import Types exposing (..)
import Ports exposing (elmToJs, jsToElm)
import Http
import Task exposing (Task)


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> jsToElm ConnectionReceived)


init : ( Model, Cmd Msg )
init =
    ( DummyModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectionReceived value ->
            case JD.decodeValue decodeConnection value of
                Ok conn ->
                    ( model, registerUserStep1 conn )

                Err e ->
                    Debug.log
                        ("Impossible condition reached. JS/Elm interface is completely broken!\n" ++ e)
                        ( model, Cmd.none )

        EffectListInt cmdFromIntList intList ->
            Debug.log ("EffectListInt") <|
                ( model, cmdFromIntList intList )

        SendResponse response ->
            ( model, sendResponseCmd response )


sendResponseCmd : Response -> Cmd msg
sendResponseCmd =
    elmToJs << encodeResponse


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


createUser : RegistrationFormDetails -> List Int -> User
createUser input saltBytes =
    let
        hexHashAndSalt =
            hashPassword input.password saltBytes
    in
        { username = input.username
        , email = input.email
        , bio = ""
        , image = ""
        , hash = hexHashAndSalt.hash
        , salt = hexHashAndSalt.salt
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


dbCreateDoc : JE.Value -> Http.Request DbCreateDocResponse
dbCreateDoc json =
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


registerUserStep1 : Connection -> Cmd Msg
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
                sendResponseCmd <|
                    Connection.errorResponse
                        Connection.BadRequest
                        conn.response

            Ok regFormData ->
                let
                    effectCallbackClosure saltBytes =
                        EffectListInt
                            (registerUserStep2 conn regFormData)
                            saltBytes
                in
                    Models.User.getRandomSaltBytes 512 effectCallbackClosure


registerUserStep2 : Connection -> RegistrationForm -> List Int -> Cmd Msg
registerUserStep2 conn regFormData saltBytes =
    let
        user =
            Debug.log "step 2, creating user" <|
                createUser
                    regFormData.user
                    (Debug.log "step 2, receiving saltBytes" saltBytes)

        dbRequest =
            dbCreateDoc <| toDatabaseJSON user

        dbTask =
            Http.toTask dbRequest

        successBody =
            JE.encode 0 <| toAuthJSON user

        handleDbResult dbResult =
            SendResponse <|
                case dbResult of
                    Ok _ ->
                        successResponse
                            (Debug.log "step 2, creating response" successBody)
                            conn.response

                    Err httpError ->
                        handleDbError conn.response httpError
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
                , body = Debug.log "DB BadStatus" httpResponse.body
            }

        Http.BadPayload jsonDecoderString httpResponse ->
            { response
                | statusCode = 500
                , body = Debug.log ("DB BadPayload\n" ++ jsonDecoderString ++ "\n") httpResponse.body
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
