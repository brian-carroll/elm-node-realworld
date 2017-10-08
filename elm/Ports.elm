port module Ports exposing (..)

import Json.Decode as JS


port elmToJs : JS.Value -> Cmd msg


port jsToElm : (JS.Value -> msg) -> Sub msg
