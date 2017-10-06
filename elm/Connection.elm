module Connection
    exposing
        ( Connection
        , Request
        , Response
        , HttpStatus(..)
        , decodeConnection
        , error
        , successResponse
        , encodeResponse
        )

import Json.Decode as JD
import Json.Encode as JE


type alias Connection =
    { request : Request
    , response : Response
    }


type alias Request =
    { method : String
    , urlPath : String
    , body : String
    }


type alias Response =
    { nodeResponseObject : JD.Value
    , statusCode : Int
    , headers : List ( String, String )
    , body : String
    }


decodeConnection : JD.Decoder Connection
decodeConnection =
    JD.map2 Connection
        (JD.field "request" decodeRequest)
        (JD.field "response" decodeResponse)


decodeRequest : JD.Decoder Request
decodeRequest =
    JD.map3 Request
        (JD.field "method" JD.string)
        (JD.field "url" JD.string)
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


encodeHeaders : List ( String, String ) -> JE.Value
encodeHeaders headers =
    headers
        |> List.map (\( k, v ) -> ( k, JE.string v ))
        |> JE.object


defaultHeaders : List ( String, String )
defaultHeaders =
    [ ( "Content-Type", "application/json; charset=utf-8" )
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


error : HttpStatus -> Response -> Response
error status response =
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
