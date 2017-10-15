module Database
    exposing
        ( createDbDoc
        , DbError
        , DbErrorResponse
        , DbCreateDocResponse
        , handleDbError
        )

import Http
import Json.Encode as JE
import Json.Decode as JD
import Types exposing (..)


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
