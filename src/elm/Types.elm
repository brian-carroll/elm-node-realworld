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
    | HandlerTaskResult Connection EndpointState
    | CollectGarbage Time


{-| Wrapper for intermediate values passed along a chain of functions in a handler
-}
type HandlerState e a
    = HandlerData a
    | AwaitingPort OutboundPortAction (JD.Value -> HandlerState e a)
    | AwaitingTask (Task Never (HandlerState e a))
    | HandlerError e


{-| A specific type of HandlerState that must eventually be produced by every endpoint
-}
type alias EndpointState =
    HandlerState EndpointError JD.Value


type alias EndpointError =
    { status : ErrorCode
    , messages : List String
    }


{-| A continuation function. Pick up where we left off after getting a value back from JS
-}
type alias Continuation =
    JD.Value -> EndpointState


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
