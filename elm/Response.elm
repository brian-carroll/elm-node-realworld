module Response exposing (..)


type alias Response =
    { statusCode : StatusCode
    , headers : List ( String, String )
    , body : String
    }


type StatusCode
    = Ok
    | NotFound


showStatusCode : StatusCode -> ( Int, String )
showStatusCode code =
    case code of
        Ok ->
            ( 200, "OK" )

        NotFound ->
            ( 404, "Not Found" )


appHeaders : List ( String, String )
appHeaders =
    [ ( "Content-Type", "application/json; charset=utf-8" )
    , ( "Access-Control-Allow-Origin", "*" )
    ]


response404 : Response
response404 =
    { statusCode = NotFound
    , headers = appHeaders
    , body = """{"errors": ["Not found"]}"""
    }


baseReponse : Response
baseReponse =
    { statusCode = Ok
    , headers = appHeaders
    , body = ""
    }
