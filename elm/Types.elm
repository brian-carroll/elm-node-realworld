module Types exposing (..)

import Json.Decode as JD
import Dict exposing (Dict)
import Time exposing (Time)


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


type InboundPortData
    = NewConnection Connection
    | JsActionResult ConnectionId JD.Value
    | InboundPortError String


{-| A continuation function. Pick up where we left off after getting a value back from JS
-}
type alias Continuation =
    Connection -> JD.Value -> Cmd Msg


type alias Secret =
    String


type alias ProgramConfig =
    { secret : Secret }


type alias Model =
    { config : ProgramConfig
    , pending :
        Dict ConnectionId ( Connection, Continuation )
    }


type Msg
    = ReceiveFromJs InboundPortData
    | SendToJs OutboundPortAction
    | CollectGarbage Time
    | ExecuteCmd (Cmd Msg)


type alias Connection =
    { request : Request
    , response : Response
    , id : ConnectionId
    }


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


type Route
    = Tags
    | Profiles String
    | ProfilesFollow String
    | Articles
    | ArticlesFeed
    | ArticleSingle String
    | ArticleFavourite String
    | ArticleComments String
    | ArticleCommentsDelete String String
    | Users
    | UsersLogin
    | User
