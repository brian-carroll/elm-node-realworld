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


handleDbError : Http.Error -> HandlerState EndpointError a
handleDbError httpError =
    case httpError of
        Http.BadUrl url ->
            HandlerError
                { status = NotFound
                , messages = [ url ]
                }

        Http.Timeout ->
            HandlerError
                { status = InternalError
                , messages = [ "DB timeout" ]
                }

        Http.NetworkError ->
            HandlerError
                { status = InternalError
                , messages = [ "DB network error" ]
                }

        Http.BadStatus httpResponse ->
            HandlerError
                { status = InternalError
                , messages = (mapDbErrors httpResponse.body)
                }

        Http.BadPayload jsonDecoderString httpResponse ->
            HandlerError
                { status = InternalError
                , messages = (mapDbErrors httpResponse.body)
                }


mapDbErrors : String -> List String
mapDbErrors errorBody =
    case JD.decodeString (JD.list (JD.field "error" JD.string)) errorBody of
        Ok messages ->
            messages

        Err _ ->
            [ "Unknown DB error" ]
