module Auth exposing (..)

import Json.Decode as JD


type alias HashAndSalt =
    { hash : String
    , salt : String
    }


decodeHashAndSalt : JD.Decoder HashAndSalt
decodeHashAndSalt =
    JD.map2 HashAndSalt
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)
