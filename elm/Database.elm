module Database exposing (..)

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


type alias DbPostBulkResponse =
    List DbPostBulkResponseItem


type alias DbPostBulkResponseItem =
    { ok : Bool
    , id : String
    , rev : String
    , error : Maybe String
    , reason : Maybe String
    }


decodeDbPostBulkResponse : JD.Decoder DbPostBulkResponse
decodeDbPostBulkResponse =
    JD.list <|
        JD.map5 DbPostBulkResponseItem
            (JD.field "ok" JD.bool)
            (JD.field "id" JD.string)
            (JD.field "rev" JD.string)
            (JD.maybe (JD.field "error" JD.string))
            (JD.maybe (JD.field "reason" JD.string))


postBulkDocs : JE.Value -> Http.Request DbPostBulkResponse
postBulkDocs json =
    Http.post
        (dbUrl ++ "/_bulk_docs")
        (Http.jsonBody json)
        decodeDbPostBulkResponse


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
                (""" {"dbErrors": """
                    ++ httpResponse.body
                    ++ "}"
                )

        Http.BadPayload jsonDecoderString httpResponse ->
            HandlerError
                Conflict
                (""" {"dbErrors": """
                    ++ httpResponse.body
                    ++ "}"
                )
