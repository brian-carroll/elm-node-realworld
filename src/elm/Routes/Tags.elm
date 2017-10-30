module Routes.Tags exposing (..)

import Routes.Parser exposing (RouteParser, Method(..), m, top, map)


type TagsRoute
    = Tags


routeParser : RouteParser (TagsRoute -> parserState) parserState
routeParser =
    map Tags (m GET top)
