module Routes exposing (..)

import UrlParser exposing (Parser, oneOf, map, s, (</>), parseString)
import Types exposing (..)
import Routes.Users exposing (UsersRoute, urlParserUsers, urlParserUser)
import Routes.Profiles exposing (ProfilesRoute)
import Routes.Articles exposing (ArticlesRoute)
import Routes.Tags exposing (TagsRoute)
import Json.Encode as JE
import Json.Decode as JD
import Task exposing (Task)
import Result


type Route
    = Tags TagsRoute
    | Profiles ProfilesRoute
    | Articles ArticlesRoute
    | Users UsersRoute


urlParser : Parser (Route -> parserState) parserState
urlParser =
    s "api"
        </> oneOf
                [ map Tags (s "tags" </> Routes.Tags.urlParser)
                , map Profiles (s "profiles" </> Routes.Profiles.urlParser)
                , map Articles (s "articles" </> Routes.Articles.urlParser)
                , map Users (s "users" </> urlParserUsers)
                , map Users (s "user" </> urlParserUser)
                ]


dispatch : ProgramConfig -> Connection -> HandlerState
dispatch config conn =
    case parseString urlParser conn.request.url of
        Just (Tags tagsRoute) ->
            HandlerData JE.null

        Just (Profiles profilesRoute) ->
            HandlerData JE.null

        Just (Articles articlesRoute) ->
            HandlerData JE.null

        Just (Users usersRoute) ->
            Routes.Users.dispatch config conn usersRoute

        Nothing ->
            HandlerError NotFound []
