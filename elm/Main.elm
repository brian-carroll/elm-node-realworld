port module Main exposing (main)

import Json.Decode as JS
import Connection exposing (Connection, decodeConnection, encodeResponse)
import Routes exposing (generateResponse)


port elmToJs : JS.Value -> Cmd msg


port jsToElm : (JS.Value -> msg) -> Sub msg


type Model
    = DummyModel


type Msg
    = ConnectionReceived JS.Value


init : ( Model, Cmd Msg )
init =
    ( DummyModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectionReceived value ->
            case JS.decodeValue decodeConnection value of
                Ok conn ->
                    let
                        req =
                            conn.request

                        res =
                            encodeResponse <|
                                generateResponse conn
                    in
                        ( model, elmToJs res )

                Err e ->
                    Debug.log
                        ("Impossible condition reached. JS/Elm interface is completely broken!\n" ++ e)
                        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> jsToElm ConnectionReceived)


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
