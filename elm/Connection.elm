module Connection
    exposing
        ( Connection
        , Request
        , Response
        , HttpStatus(..)
        , decodeConnection
        , errorResponse
        , successResponse
        , encodeResponse
        )

import Json.Decode as JD
import Json.Encode as JE
import Dict exposing (Dict)


type alias Connection =
    { request : Request
    , response : Response
    }


type alias Request =
    { method : String
    , urlPath : String
    , headers : Dict String String
    , body : String
    }


type alias Response =
    { nodeResponseObject : JD.Value
    , statusCode : Int
    , headers : Dict String String
    , body : String
    }


decodeConnection : JD.Decoder Connection
decodeConnection =
    JD.map2 Connection
        (JD.field "request" decodeRequest)
        (JD.field "response" decodeResponse)


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


type HttpStatus
    = Ok
    | NotFound
    | MethodNotAllowed
    | InternalError


mapHttpStatus : HttpStatus -> ( Int, String )
mapHttpStatus code =
    case code of
        Ok ->
            ( 200, "OK" )

        NotFound ->
            ( 404, "Not Found" )

        MethodNotAllowed ->
            ( 405, "Method Not Allowed" )

        InternalError ->
            ( 500, "Internal Error" )


errorResponse : HttpStatus -> Response -> Response
errorResponse status response =
    let
        ( num, str ) =
            mapHttpStatus status
    in
        { response
            | statusCode = num
            , body = str
        }


successResponse : String -> Response -> Response
successResponse json response =
    { response
        | statusCode = 200
        , body = json
    }
