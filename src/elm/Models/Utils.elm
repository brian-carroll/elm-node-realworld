module Models.Utils exposing (..)

import Regex
import Json.Decode as JD exposing (Decoder)


matchesRegex : Regex.Regex -> (String -> a) -> String -> Decoder a
matchesRegex r constructor s =
    if Regex.contains r s then
        JD.succeed (constructor s)
    else
        JD.fail (toString (constructor s) ++ " is invalid")
