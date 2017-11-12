module Framework.Connection
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
    JD.map5 Connection
        (JD.field "request" decodeRequest)
        (JD.field "response" decodeResponse)
        (JD.succeed connectionId)
        (JD.field "timestamp" JD.float)
        (JD.field "dbClient" JD.value)


decodeRequest : JD.Decoder Request
decodeRequest =
    JD.map4 Request
        (JD.field "method" JD.string)
        (JD.field "url" JD.string)
        (JD.field "headers" <| JD.dict JD.string)
        (JD.field "body" JD.string)


decodeResponse : JD.Decoder Response
decodeResponse =
    JD.map4 Response
        JD.value
        (JD.succeed 200)
        (JD.succeed defaultHeaders)
        (JD.succeed "")


encodeResponse : Connection -> JE.Value
encodeResponse conn =
    JE.object
        [ ( "nodeResponseObject", conn.response.nodeResponseObject )
        , ( "statusCode", JE.int conn.response.statusCode )
        , ( "headers", encodeHeaders conn.response.headers )
        , ( "body", JE.string conn.response.body )
        , ( "dbClient", conn.dbClient )
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


mapHttpStatus : ErrorCode -> Int
mapHttpStatus code =
    case code of
        BadRequest ->
            400

        Unauthorized ->
            401

        Forbidden ->
            403

        NotFound ->
            404

        MethodNotAllowed ->
            405

        UnprocessableEntity ->
            422

        InternalError ->
            500


successResponse : JE.Value -> Connection -> Connection
successResponse json conn =
    let
        res =
            conn.response
    in
        { conn
            | response =
                { res
                    | body = JE.encode 0 json
                }
        }


errorResponse : EndpointError -> Connection -> Connection
errorResponse { status, messages } conn =
    let
        res =
            conn.response

        json =
            JE.object
                [ ( "errors"
                  , JE.object
                        [ ( "body"
                          , JE.list (List.map JE.string messages)
                          )
                        ]
                  )
                ]
    in
        { conn
            | response =
                { res
                    | body = JE.encode 0 json
                    , statusCode = mapHttpStatus status
                }
        }
