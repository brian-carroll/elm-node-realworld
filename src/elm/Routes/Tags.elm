module Routes.Tags exposing (..)

import Routes.Parser exposing (Parser, Method(..), m, top, map)


type TagsRoute
    = Tags


routeParser : Parser (TagsRoute -> parserState) parserState
routeParser =
    map Tags (m GET top)
