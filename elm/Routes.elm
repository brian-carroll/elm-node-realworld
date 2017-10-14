module Routes exposing (..)

import Connection exposing (HttpStatus(..), errorResponse, successResponse)
import UrlParser exposing (Parser, oneOf, map, s, (</>))
import Types exposing (..)
import Routes.Users exposing (urlParserUsers, urlParserUser)
import Routes.Profiles
import Routes.Articles
import Routes.Tags


type Route
    = Tags
    | Profiles ProfilesRoute
    | Articles ArticlesRoute
    | Users UsersRoute


urlParser : Parser (Route -> parserState) parserState
urlParser =
    oneOf
        [ map Tags (s "tags" </> Routes.Tags.urlParser)
        , map Profiles (s "profiles" </> Routes.Profiles.urlParser)
        , map Articles (s "articles" </> Routes.Articles.urlParser)
        , map Users (s "users" </> urlParserUsers)
        , map Users (s "user" </> urlParserUser)
        ]


routeDispatch : Route -> Method -> Connection -> HandlerState
routeDispatch route method conn =
    case route of
        Tags ->
            ( Nothing, Cmd.none )

        Profiles profilesRoute ->
            ( Nothing, Cmd.none )

        Articles articlesRoute ->
            ( Nothing, Cmd.none )

        Users usersRoute ->
            ( Nothing, Cmd.none )


userRouteDispatch : UsersRoute -> Method -> ( Maybe ( Connection, Continuation ), Cmd Msg )
userRouteDispatch route method =
    case ( route, method ) of
        ( Register, Post ) ->
            ( Nothing, Cmd.none )

        ( Login, Post ) ->
            ( Nothing, Cmd.none )

        ( CurrentUser, Get ) ->
            ( Nothing, Cmd.none )

        ( CurrentUser, Put ) ->
            ( Nothing, Cmd.none )

        _ ->
            ( Nothing, Cmd.none )
