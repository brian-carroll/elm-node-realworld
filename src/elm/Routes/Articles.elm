module Routes.Articles exposing (ArticlesRoute, routeParser)

import Routes.Parser exposing (RouteParser, Parser, Method(..), m, (</>), s, string, map, oneOf, parseRoute, top)
import Types exposing (..)


type Slug
    = Slug String


type CommentId
    = CommentId String


type ArticlesRoute
    = ListArticles
    | FeedArticles
    | GetArticle Slug
    | CreateArticle
    | UpdateArticle Slug
    | DeleteArticle Slug
    | AddComment Slug
    | GetComments Slug
    | DeleteComment Slug CommentId
    | Favourite Slug
    | Unfavourite Slug


slug : Parser m (Slug -> state) state
slug =
    map Slug string


commentId : Parser m (CommentId -> state) state
commentId =
    map CommentId string


routeParser : RouteParser (ArticlesRoute -> state) state
routeParser =
    oneOf
        [ map ListArticles (m GET top)
        , map FeedArticles (m GET (s "feed"))
        , map GetArticle (m GET slug)
        , map CreateArticle (m POST top)
        , map UpdateArticle (m PUT slug)
        , map DeleteArticle (m DELETE slug)
        , map AddComment (m POST (slug </> s "comments"))
        , map GetComments (m GET (slug </> s "comments"))
        , map DeleteComment (m DELETE (slug </> s "comments" </> commentId))
        , map Favourite (m POST (slug </> s "favourite"))
        , map Unfavourite (m DELETE (slug </> s "favourite"))
        ]
