port module Main exposing (main)

import Json.Decode
import Json.Encode


port elmToJs : String -> Cmd msg


port jsToElm : (String -> msg) -> Sub msg


type Model
    = EchoString String


type Msg
    = Echo String


init : ( Model, Cmd Msg )
init =
    ( EchoString "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo str ->
            ( EchoString str, elmToJs str )


subscriptions : Model -> Sub Msg
subscriptions =
    (\_ -> jsToElm Echo)


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
