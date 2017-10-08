module Types exposing (..)

import Json.Decode as JS
import Dict exposing (Dict)


type Model
    = DummyModel


type Msg
    = ConnectionReceived JS.Value
    | EffectListInt (List Int -> Cmd Msg) (List Int)
    | SendResponse Response


type PipelineEffect
    = Execute Cmd
    | Respond Response


type alias Connection =
    { request : Request
    , response : Response
    }


type alias Request =
    { method : Method
    , url : String
    , headers : Dict String String
    , body : String
    }


type alias Response =
    { nodeResponseObject : JS.Value
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
