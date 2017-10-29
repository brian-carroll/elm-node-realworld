module Routes.Tags exposing (..)

import Routes.Parser exposing (Parser, top, map)


{-
   get  '/tags/'
-}


type TagsRoute
    = Tags


urlParser : Parser (TagsRoute -> parserState) parserState
urlParser =
    map Tags top
