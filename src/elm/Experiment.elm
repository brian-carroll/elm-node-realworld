module Experiment exposing (..)

import Json.Decode as JD
import Dict exposing (Dict)
import Time exposing (Time)
import Task exposing (Task)
import Result


type alias ConnectionId =
    ( Time, Int )


{-| A continuation function. Pick up where we left off after getting a value back from JS
-}
type alias Continuation =
    Connection -> JD.Value -> HandlerState


type alias Connection =
    { request : Request
    , response : Response
    , id : ConnectionId
    , timestamp : Time
    }


type ErrorCode
    = Unauthorized
    | Forbidden
    | NotFound
    | MethodNotAllowed
    | UnprocessableEntity
    | InternalError


type Method
    = Get
    | Post
    | Put
    | Delete


type alias Request =
    { method : Method
    , url : String
    , headers : Dict String String
    , body : String
    }


type alias Response =
    { nodeResponseObject : JD.Value
    , statusCode : Int
    , headers : Dict String String
    , body : String
    }


type OutboundPortAction
    = RespondToClient
    | HashPassword String
    | CheckPassword
        { hash : String
        , salt : String
        , plainText : String
        }



-- type HandlerError


type alias Secret =
    String


type Username
    = Username String


type alias JwtPayload =
    { username : Username
    , exp : Int
    }



-- requireAuth : Secret -> Connection -> Result HandlerState JwtPayload
-- requireAuth secret conn =
--     if conn.timestamp > 123456 then
--         Ok { username = Username "dude", exp = 123456 }
--     else
--         Err (HandlerError Unauthorized [ "Invalid Token" ])
-- getCurrentUser : Secret -> Connection -> Result HandlerError HandlerState
-- getCurrentUser secret conn =
--     let
--         getUserDoc : Username -> HandlerState
--         getUserDoc username =
--             AwaitingTask
--                 (findByUsername username
--                     |> Task.andThen (Task.succeed << generateResponse)
--                     |> Task.onError (Task.succeed << handleDbError)
--                 )
--         generateResponse : User -> HandlerState
--         generateResponse user =
--             HandlerSuccess <|
--                 JE.object <|
--                     [ ( "user"
--                       , toAuthJSON secret conn.timestamp user
--                       )
--                     ]
--     in
--         requireAuth secret conn
--             |> andThen (\jwtPayload -> Ok jwtPayload.username)
--             |> andThen


type alias AppError =
    ( ErrorCode, List String )


type alias AppHandlerState =
    HandlerState AppError JD.Value


type HandlerState e a
    = HandlerData a
    | AwaitingPort OutboundPortAction (JD.Value -> HandlerState e a)
    | AwaitingTask (Task Never (HandlerState e a))
    | HandlerError e


andThen : (a -> HandlerState x b) -> HandlerState x a -> HandlerState x b
andThen nextStage state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            nextStage data

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> andThen nextStage)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (andThen nextStage))


onError : (y -> HandlerState x a) -> HandlerState x (Result y a) -> HandlerState x a
onError liftError state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData result ->
            case result of
                Ok data ->
                    HandlerData data

                Err e ->
                    liftError e

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> onError liftError)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (onError liftError))


try : (a -> Result x b) -> (x -> HandlerState y b) -> HandlerState y a -> HandlerState y b
try f liftError state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            case f data of
                Ok newData ->
                    HandlerData newData

                Err e ->
                    liftError e

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> try f liftError)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (try f liftError))


exitWith errCode errString =
    HandlerError ( errCode, [ errString ] )


onErrorExitWith : ErrorCode -> HandlerState AppError (Result String a) -> HandlerState AppError a
onErrorExitWith errCode =
    onError (\e -> HandlerError ( errCode, [ e ] ))


simplePipeline : JD.Value -> HandlerState ( ErrorCode, List String ) String
simplePipeline inputData =
    HandlerData inputData
        |> andThen (HandlerData << JD.decodeValue JD.string)
        |> onError (\e -> HandlerError ( InternalError, [ e ] ))


simplePipeline2 : JD.Value -> HandlerState ( ErrorCode, List String ) String
simplePipeline2 inputData =
    HandlerData inputData
        |> andThen (HandlerData << JD.decodeValue JD.string)
        |> onErrorExitWith InternalError
        |> andThen (\d -> HandlerData (d ++ "hello"))


simplePipeline3 : JD.Value -> HandlerState ( ErrorCode, List String ) String
simplePipeline3 inputData =
    HandlerData inputData
        |> try (JD.decodeValue JD.string)
            (\e -> HandlerError ( InternalError, [ e ] ))


simplePipeline4 : JD.Value -> HandlerState ( ErrorCode, List String ) String
simplePipeline4 inputData =
    HandlerData inputData
        |> andThen (HandlerData << JD.decodeValue JD.string)
        |> onError (exitWith InternalError)
        |> andThen (\d -> HandlerData (d ++ "hello"))
        |> andThen (\d -> AwaitingPort (HashPassword d) HandlerData)
        |> andThen (HandlerData << JD.decodeValue JD.string)
        |> onError (exitWith InternalError)
