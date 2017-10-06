module Routes exposing (..)

import Connection exposing (Connection, Response, HttpStatus(..), errorResponse, successResponse)
import UrlParser exposing (Parser, (</>), s, string, map, oneOf, parseString)


type Method
    = Get
    | Post
    | Put
    | Delete


type Permissions
    = AuthRequired
    | AuthOptional


type Route
    = Tags
    | Profiles String
    | ProfilesFollow String
    | Articles
    | ArticlesFeed
    | ArticleSingle String
    | ArticleFavourite String
    | ArticleComments String
    | ArticleCommentsDelete String String
    | Users
    | UsersLogin
    | User


type Endpoint
    = Endpoint Route Method Permissions (String -> Response)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Tags (s "tags")
        , map Profiles (s "profiles" </> string)
        , map ProfilesFollow (s "profiles" </> string </> s "follow")
        , map Articles (s "articles")
        , map ArticlesFeed (s "articles" </> s "feed")
        , map ArticleSingle (s "articles" </> string)
        , map ArticleFavourite (s "articles" </> string </> s "favourite")
        , map ArticleComments (s "articles" </> string </> s "comments")
        , map ArticleCommentsDelete (s "articles" </> string </> s "comments" </> string)
        , map Users (s "users")
        , map UsersLogin (s "users" </> s "login")
        , map User (s "user")
        ]


methodParser : String -> Maybe Method
methodParser s =
    case s of
        "GET" ->
            Just Get

        "POST" ->
            Just Post

        "PUT" ->
            Just Put

        "DELETE" ->
            Just Delete

        _ ->
            Nothing



-- All of this stuff should really be returning Cmds or Tasks or something
-- Tasks are probably nicer, they usually are.
-- In cases where we don't need async, just use the `successResponse` task
-- Have to convert to Cmd in the top-level update, so just do it there


generateResponse : Connection -> Response
generateResponse conn =
    case methodParser conn.request.method of
        Nothing ->
            errorResponse MethodNotAllowed conn.response

        Just method ->
            case parseString routeParser conn.request.urlPath of
                Nothing ->
                    errorResponse NotFound conn.response

                Just route ->
                    let
                        routeHandler =
                            selectRouteHandler route
                    in
                        routeHandler method conn


selectRouteHandler : Route -> (Method -> Connection -> Response)
selectRouteHandler route =
    case route of
        Tags ->
            \_ conn -> successResponse "tags" conn.response

        Profiles username ->
            \_ conn -> successResponse ("profiles " ++ username) conn.response

        ProfilesFollow username ->
            \_ conn -> successResponse ("profiles follow " ++ username) conn.response

        Articles ->
            \_ conn -> successResponse "articles" conn.response

        ArticlesFeed ->
            \_ conn -> successResponse "articles feed" conn.response

        ArticleSingle articleId ->
            \_ conn -> successResponse ("single article " ++ articleId) conn.response

        ArticleFavourite articleId ->
            \_ conn -> successResponse ("article favourite " ++ articleId) conn.response

        ArticleComments articleId ->
            \_ conn -> successResponse ("article comments " ++ articleId) conn.response

        ArticleCommentsDelete articleId commentId ->
            \_ conn -> successResponse ("delete comment " ++ commentId ++ " from " ++ articleId) conn.response

        Users ->
            \_ conn -> successResponse "users" conn.response

        UsersLogin ->
            \_ conn -> successResponse "login" conn.response

        User ->
            \_ conn -> successResponse "user" conn.response
