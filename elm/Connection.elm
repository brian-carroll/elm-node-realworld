module Connection
    exposing
        ( decodeConnection
        , errorResponse
        , successResponse
        , encodeResponse
        )

import Json.Decode as JD
import Json.Encode as JE
import Dict exposing (Dict)
import Types exposing (..)


decodeConnection : ConnectionId -> JD.Decoder Connection
decodeConnection connectionId =
    JD.map3 Connection
        (JD.field "request" decodeRequest)
        (JD.field "response" decodeResponse)
        (JD.succeed connectionId)


decodeRequest : JD.Decoder Request
decodeRequest =
    JD.map4 Request
        (JD.field "method" decodeMethod)
        (JD.field "url" JD.string)
        (JD.field "headers" <| JD.dict JD.string)
        (JD.field "body" JD.string)



-- decodeRoute : JD.Decoder Route
-- decodeRoute =
--     JD.string
--         |> JD.andThen
--             (\s ->
--                 case routeParser s of
--                     Just route ->
--                         JD.succeed route
--                     Nothing ->
--                         JD.fail "Route"
--             )


decodeMethod : JD.Decoder Method
decodeMethod =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "GET" ->
                        JD.succeed Get

                    "POST" ->
                        JD.succeed Post

                    "PUT" ->
                        JD.succeed Put

                    "DELETE" ->
                        JD.succeed Delete

                    _ ->
                        JD.fail "Method"
            )


decodeResponse : JD.Decoder Response
decodeResponse =
    JD.map4 Response
        JD.value
        (JD.succeed 200)
        (JD.succeed defaultHeaders)
        (JD.succeed "")


encodeResponse : Response -> JE.Value
encodeResponse response =
    JE.object
        [ ( "nodeResponseObject", response.nodeResponseObject )
        , ( "statusCode", JE.int response.statusCode )
        , ( "headers", encodeHeaders response.headers )
        , ( "body", JE.string response.body )
        ]


encodeHeaders : Dict String String -> JE.Value
encodeHeaders headers =
    headers
        |> Dict.map (\_ -> JE.string)
        |> Dict.toList
        |> JE.object


defaultHeaders : Dict String String
defaultHeaders =
    Dict.fromList
        [ ( "Content-Type", "application/json" )
        , ( "Accept", "application/json" )
        ]


mapHttpStatus : HttpStatus -> ( Int, String )
mapHttpStatus code =
    case code of
        HttpOk ->
            ( 200, "OK" )

        BadRequest ->
            ( 400, "Bad Request" )

        NotFound ->
            ( 404, "Not Found" )

        MethodNotAllowed ->
            ( 405, "Method Not Allowed" )

        RequestTimeout ->
            ( 408, "Request Timeout" )

        Conflict ->
            ( 409, "Conflict" )

        InternalError ->
            ( 500, "Internal Error" )

        ServiceUnavailable ->
            ( 503, "Service Unavailable" )


errorResponse : HttpStatus -> String -> Response -> Response
errorResponse status body response =
    let
        ( num, str ) =
            mapHttpStatus status
    in
        { response
            | statusCode = num
            , body = body
        }


successResponse : String -> Response -> Response
successResponse str response =
    { response
        | statusCode = 200
        , body = str
    }
