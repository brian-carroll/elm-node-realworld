module Types exposing (..)

import Json.Decode as JD
import Dict exposing (Dict)
import Time exposing (Time)
import Task exposing (Task)


type alias ConnectionId =
    ( Time, Int )


type alias JsInterface =
    { connectionId : ConnectionId
    , tag : String
    , payload : JD.Value
    }


type OutboundPortAction
    = RespondToClient
    | HashPassword String
    | CheckPassword
        { hash : String
        , salt : String
        , plainText : String
        }


type InboundPortData
    = NewConnection Connection
    | JsActionResult ConnectionId JD.Value
    | InboundPortError String


type alias Secret =
    String


type alias ProgramConfig =
    { secret : Secret }


type alias PendingHandlers =
    Dict ConnectionId ( Connection, Continuation )


type alias State =
    { config : ProgramConfig
    , pending : PendingHandlers
    }


type Msg
    = ReceiveFromJs JsInterface
    | HandlerTaskResult Connection HandlerState
    | CollectGarbage Time


type HandlerState
    = HandlerSuccess JD.Value
    | HandlerError ErrorCode (List String)
    | AwaitingPort OutboundPortAction Continuation
    | AwaitingTask (Task Never HandlerState)


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


type Method
    = Get
    | Post
    | Put
    | Delete