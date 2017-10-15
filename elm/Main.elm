port module Main exposing (main)

import Json.Decode as JD
import Json.Encode as JE
import Connection exposing (decodeConnection, errorResponse, successResponse, encodeResponse)
import Models.User exposing (..)
import Types exposing (..)
import Http
import Task exposing (Task)
import Dict
import Time exposing (Time)


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


main : Program ProgramConfig State Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch <|
        (jsToElm ReceiveFromJs)
            :: (if Dict.isEmpty state.pending then
                    []
                else
                    [ Time.every Time.second CollectGarbage ]
               )


init : ProgramConfig -> ( State, Cmd Msg )
init config =
    ( { config = config
      , pending = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        ( pending, cmd ) =
            case msg of
                ReceiveFromJs jsData ->
                    updateJsData state jsData

                HandlerTaskResult conn handlerState ->
                    updateHandlerState conn state.pending handlerState

                CollectGarbage now ->
                    updateGarbageCollection state.pending now
    in
        ( { state | pending = pending }, cmd )


dumpPendingHandler : ( Connection, a ) -> Cmd Msg
dumpPendingHandler ( conn, _ ) =
    jsActionCmd RespondToClient
        { conn
            | response =
                errorResponse RequestTimeout "Timeout" conn.response
        }


updateGarbageCollection : PendingHandlers -> Time -> ( PendingHandlers, Cmd Msg )
updateGarbageCollection pending now =
    let
        predicate ( timestamp, seq ) val =
            now - timestamp > Time.second

        ( keep, dump ) =
            Dict.partition predicate pending

        dumpCmds =
            Dict.foldl
                (\key value acc -> dumpPendingHandler value :: acc)
                []
                (Debug.log "Collecting garbage" dump)
    in
        ( keep, Cmd.batch dumpCmds )


updateJsData : State -> JsInterface -> ( PendingHandlers, Cmd Msg )
updateJsData state jsData =
    case decodeDataFromJs jsData of
        NewConnection conn ->
            (registerUserStep1 state.config.secret conn)
                |> updateHandlerState conn state.pending

        JsActionResult connId jsValue ->
            case Dict.get connId state.pending of
                Just ( connection, continueWith ) ->
                    continueWith connection jsValue
                        |> updateHandlerState
                            connection
                            (Dict.remove connId state.pending)

                Nothing ->
                    Debug.log ("Pending connection not found:\n" ++ toString connId)
                        ( state.pending, Cmd.none )

        InboundPortError str ->
            -- TODO: do something sensible here. Try to respond to client.
            ( state.pending, Cmd.none )


updateHandlerState : Connection -> PendingHandlers -> HandlerState -> ( PendingHandlers, Cmd Msg )
updateHandlerState conn pendingHandlers handlerState =
    case handlerState of
        HandlerSuccess json ->
            ( pendingHandlers
            , jsActionCmd RespondToClient
                { conn
                    | response =
                        successResponse (JE.encode 0 json) conn.response
                }
            )

        HandlerError httpStatus str ->
            ( pendingHandlers
            , jsActionCmd RespondToClient
                { conn
                    | response =
                        errorResponse httpStatus str conn.response
                }
            )

        AwaitingPort outboundPortAction continuation ->
            ( Dict.insert conn.id ( conn, continuation ) pendingHandlers
            , jsActionCmd outboundPortAction conn
            )

        AwaitingTask task ->
            ( pendingHandlers
            , Task.perform (HandlerTaskResult conn) task
            )


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
    { id = Nothing
    , username = formDetails.username
    , email = formDetails.email
    , bio = ""
    , image = ""
    , hash = hashAndSalt.hash
    , salt = hashAndSalt.salt
    }


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


registerUserStep1 : Secret -> Connection -> HandlerState
registerUserStep1 secret conn =
    let
        inputResult =
            Debug.log "step 1, decoding input JSON" <|
                JD.decodeString
                    decodeCreateUserInputData
                    conn.request.body
    in
        case inputResult of
            Err str ->
                HandlerError Types.BadRequest str

            Ok regFormData ->
                AwaitingPort
                    (HashPassword regFormData.user.password)
                    (registerUserStep2 secret regFormData)


type alias HashAndSalt =
    { hash : String
    , salt : String
    }


decodeHashAndSalt : JD.Decoder HashAndSalt
decodeHashAndSalt =
    JD.map2 HashAndSalt
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


registerUserStep2 : Secret -> RegistrationForm -> Connection -> JD.Value -> HandlerState
registerUserStep2 secret regFormData conn jsHashAndSalt =
    case JD.decodeValue decodeHashAndSalt jsHashAndSalt of
        Ok hashAndSalt ->
            registerUserStep3 secret regFormData conn hashAndSalt

        Err e ->
            HandlerError InternalError e



-- TODO: generalise the above function to map Result to HandlerState


registerUserStep3 : Secret -> RegistrationForm -> Connection -> HashAndSalt -> HandlerState
registerUserStep3 secret regFormData conn hashAndSalt =
    let
        user =
            createUser
                regFormData.user
                hashAndSalt

        now =
            Tuple.first conn.id

        dbTask =
            user
                |> toDatabaseJSON
                |> createDbDoc
                |> Http.toTask
                |> Task.andThen (Task.succeed << handleDbUserCreatedSuccess secret now user)
                |> Task.onError (Task.succeed << handleDbError)
    in
        AwaitingTask dbTask


handleDbUserCreatedSuccess : Secret -> Time -> User -> DbCreateDocResponse -> HandlerState
handleDbUserCreatedSuccess secret now user dbResponse =
    case toAuthJSON secret now { user | id = Just dbResponse.id } of
        Just json ->
            HandlerSuccess json

        Nothing ->
            HandlerError InternalError "DB user created but has no ID"


handleDbError : Http.Error -> HandlerState
handleDbError httpError =
    case httpError of
        Http.BadUrl url ->
            HandlerError NotFound url

        Http.Timeout ->
            HandlerError RequestTimeout "DB timeout"

        Http.NetworkError ->
            HandlerError RequestTimeout "DB network error"

        Http.BadStatus httpResponse ->
            HandlerError
                InternalError
                ("DB error: " ++ toString httpResponse)

        Http.BadPayload jsonDecoderString httpResponse ->
            HandlerError
                InternalError
                ("DB error: " ++ toString httpResponse)
