module Routes exposing (..)

import Request exposing (Request)
import Response exposing (Response, HttpStatus(..), error, success)
import UrlParser exposing (Parser, (</>), s, string, map, oneOf, parseString)


type Method
    = Get
    | Post
    | Put
    | Delete


type Permissions
    = AuthRequired
    | AuthOptional


type Username
    = Username String


type ArticleId
    = ArticleId String


type CommentId
    = CommentId String


type Route
    = Tags
    | Profiles Username
    | ProfilesFollow Username
    | Articles
    | ArticlesFeed
    | ArticleSingle ArticleId
    | ArticleFavourite ArticleId
    | ArticleComments ArticleId
    | ArticleCommentsDelete ArticleId CommentId
    | Users
    | UsersLogin
    | User


type Endpoint
    = Endpoint Route Method Permissions (String -> Response)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Tags (s "tags")
        , map (Profiles << Username) (s "profiles" </> string)
        , map (ProfilesFollow << Username) (s "profiles" </> string </> s "follow")
        , map Articles (s "articles")
        , map ArticlesFeed (s "articles" </> s "feed")
        , map (ArticleSingle << ArticleId) (s "articles" </> string)
        , map (ArticleFavourite << ArticleId) (s "articles" </> string </> s "favourite")
        , map (ArticleComments << ArticleId) (s "articles" </> string </> s "comments")
        , map (\a c -> ArticleCommentsDelete (ArticleId a) (CommentId c)) (s "articles" </> string </> s "comments" </> string)
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
-- In cases where we don't need async, just use the `success` task
-- Have to convert to Cmd in the top-level update, so just do it there


processRequest : Request -> Response
processRequest req =
    case methodParser req.method of
        Nothing ->
            error MethodNotAllowed

        Just method ->
            case parseString routeParser req.urlPath of
                Nothing ->
                    error NotFound

                Just route ->
                    selectRoute method route req.body


selectRoute : Method -> Route -> String -> Response
selectRoute method route =
    case route of
        Tags ->
            success "tags"

        Profiles username ->
            success <| "profiles " ++ username

        ProfilesFollow username ->
            success <| "profiles follow " ++ username

        Articles ->
            success "articles"

        ArticlesFeed ->
            success "articles feed"

        ArticleSingle articleId ->
            success <| "single article " ++ articleId

        ArticleFavourite articleId ->
            success <| "article favourite " ++ articleId

        ArticleComments articleId ->
            success <| "article comments " ++ articleId

        ArticleCommentsDelete articleId commentId ->
            success <| "delete article " ++ articleId

        Users ->
            success "users"

        UsersLogin ->
            success "login"

        User ->
            success "user"
