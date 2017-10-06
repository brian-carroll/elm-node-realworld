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
                    selectRoute route conn


selectRoute : Route -> Connection -> Response
selectRoute route conn =
    conn.response
        |> case route of
            Tags ->
                successResponse "tags"

            Profiles username ->
                successResponse <| "profiles " ++ username

            ProfilesFollow username ->
                successResponse <| "profiles follow " ++ username

            Articles ->
                successResponse "articles"

            ArticlesFeed ->
                successResponse "articles feed"

            ArticleSingle articleId ->
                successResponse <| "single article " ++ articleId

            ArticleFavourite articleId ->
                successResponse <| "article favourite " ++ articleId

            ArticleComments articleId ->
                successResponse <| "article comments " ++ articleId

            ArticleCommentsDelete articleId commentId ->
                successResponse <| "delete comment " ++ commentId ++ " from " ++ articleId

            Users ->
                successResponse "users"

            UsersLogin ->
                successResponse "login"

            User ->
                successResponse "user"
