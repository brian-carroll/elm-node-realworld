module Routes exposing (..)

-- external imports

import Json.Encode as JE


-- local imports

import Types exposing (..)
import Routes.Parser exposing (Parser, ParseError(..), Route, oneOf, map, s, (</>), parseRoute)
import Routes.Users exposing (UsersRoute, urlParserUsers, urlParserUser)
import Routes.Profiles exposing (ProfilesRoute)
import Routes.Articles exposing (ArticlesRoute)
import Routes.Tags exposing (TagsRoute)


type Route
    = Tags TagsRoute
    | Profiles ProfilesRoute
    | Articles ArticlesRoute
    | Users UsersRoute


routeParser : Parser (Route -> parserState) parserState
routeParser =
    s "api"
        </> oneOf
                [ map Tags (s "tags" </> Routes.Tags.routeParser)
                , map Profiles (s "profiles" </> Routes.Profiles.routeParser)
                , map Articles (s "articles" </> Routes.Articles.routeParser)
                , map Users (s "users" </> urlParserUsers)
                , map Users (s "user" </> urlParserUser)
                ]


dispatch : ProgramConfig -> Connection -> EndpointState
dispatch config conn =
    let
        route =
            Debug.log "Route" <|
                parseRoute
                    routeParser
                    (Route conn.request.method conn.request.url)
    in
        case route of
            Ok subroute ->
                case subroute of
                    Tags tagsRoute ->
                        HandlerData JE.null

                    Profiles profilesRoute ->
                        Routes.Profiles.dispatch config conn profilesRoute

                    Articles articlesRoute ->
                        HandlerData JE.null

                    Users usersRoute ->
                        Routes.Users.dispatch config conn usersRoute

            Err parseError ->
                case parseError of
                    UrlMismatch ->
                        HandlerError { status = NotFound, messages = [] }

                    MethodMismatch ->
                        HandlerError { status = MethodNotAllowed, messages = [] }

                    BadQueryString ->
                        HandlerError { status = BadRequest, messages = [] }
