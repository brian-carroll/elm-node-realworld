port module Main exposing (main)

import SqlToElm exposing (convert)
import SqlToElm.Types exposing (File)


-- Json.Decode is implicitly required for ports!

import Json.Decode


type alias Config =
    { sqlDir : String
    , elmDir : String
    }


type Msg
    = IncomingFile File


type alias Output =
    { path : String
    , body : String
    , error : String
    }


port jsToElm : (File -> msg) -> Sub msg


port elmToJs : Output -> Cmd msg


main : Program Config Config Msg
main =
    Platform.programWithFlags
        { init = \config -> ( config, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Config -> Sub Msg
subscriptions _ =
    jsToElm IncomingFile


update : Msg -> Config -> ( Config, Cmd Msg )
update msg config =
    case msg of
        IncomingFile sqlFile ->
            ( config
            , elmToJs <|
                case convert config sqlFile of
                    Ok elmFile ->
                        { path = elmFile.path
                        , body = elmFile.body
                        , error = ""
                        }

                    Err e ->
                        { path = ""
                        , body = ""
                        , error = (toString e)
                        }
            )
