module Routes.Tags exposing (..)

import Framework.RouteParser exposing (Parser, Method(..), m, top, map)


type TagsRoute
    = Tags


routeParser : Parser (TagsRoute -> parserState) parserState
routeParser =
    map Tags (m GET top)
