module Routes.Tags exposing (..)

import UrlParser exposing (Parser, top)
import Types exposing (..)


{-
   get  '/tags/'
-}


urlParser : Parser (TagsRoute -> parserState) parserState
urlParser =
    top
