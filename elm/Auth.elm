module Auth exposing (..)


type alias HashAndSalt =
    { hash : String
    , salt : String
    }
