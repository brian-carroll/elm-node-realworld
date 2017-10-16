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
    JD.map4 Connection
        (JD.field "request" decodeRequest)
        (JD.field "response" decodeResponse)
        (JD.succeed connectionId)
        (JD.succeed <| Tuple.first connectionId)


decodeRequest : JD.Decoder Request
decodeRequest =
    JD.map4 Request
        (JD.field "method" decodeMethod)
        (JD.field "url" JD.string)
        (JD.field "headers" <| JD.dict JD.string)
        (JD.field "body" JD.string)


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


mapHttpStatus : ErrorCode -> Int
mapHttpStatus code =
    case code of
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


errorResponse : ErrorCode -> List String -> Connection -> Connection
errorResponse httpStatus errors conn =
    let
        res =
            conn.response

        json =
            JE.object
                [ ( "errors"
                  , JE.object
                        [ ( "body"
                          , JE.list (List.map JE.string errors)
                          )
                        ]
                  )
                ]
    in
        { conn
            | response =
                { res
                    | body = JE.encode 0 json
                    , statusCode = mapHttpStatus httpStatus
                }
        }
