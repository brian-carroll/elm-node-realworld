module Routes.Articles exposing (ArticlesRoute, routeParser, dispatch)

-- external imports

import Json.Encode as JE


-- local imports

import Routes.Parser exposing (Parser, Method(..), m, (</>), s, string, map, oneOf, parseRoute, top)
import Types exposing (..)
import Models.User exposing (User, UserId)
import Models.Article exposing (Article, Slug(..))
import HandlerState exposing (andThen, map2, wrapErrString)
import Routes.Api exposing (encodeDate)


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


slug : Parser (Slug -> state) state
slug =
    map Slug string


commentId : Parser (CommentId -> state) state
commentId =
    map CommentId string


routeParser : Parser (ArticlesRoute -> state) state
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


dispatch : ArticlesRoute -> EndpointState
dispatch route =
    case route of
        ListArticles ->
            HandlerData JE.null

        FeedArticles ->
            HandlerData JE.null

        GetArticle slug ->
            getArticle slug

        CreateArticle ->
            HandlerData JE.null

        UpdateArticle slug ->
            HandlerData JE.null

        DeleteArticle slug ->
            HandlerData JE.null

        AddComment slug ->
            HandlerData JE.null

        GetComments slug ->
            HandlerData JE.null

        DeleteComment slug commentId ->
            HandlerData JE.null

        Favourite slug ->
            HandlerData JE.null

        Unfavourite slug ->
            HandlerData JE.null



{-
   {
     "article": {
       "slug": "how-to-train-your-dragon",
       "title": "How to train your dragon",
       "description": "Ever wonder how?",
       "body": "It takes a Jacobian",
       "tagList": ["dragons", "training"],
       "createdAt": "2016-02-18T03:22:56.637Z",
       "updatedAt": "2016-02-18T03:48:35.824Z",
       "favorited": false,
       "favoritesCount": 0,
       "author": {
         "username": "jake",
         "bio": "I work at statefarm",
         "image": "https://i.stack.imgur.com/xHWG8.jpg",
         "following": false
       }
     }
   }
-}


encodeArticle : Article -> JE.Value -> JE.Value
encodeArticle article authorProfileObj =
    JE.object
        [ ( "slug", Models.Article.encodeSlug article.slug )
        , ( "title", JE.string article.title )
        , ( "description", JE.string article.description )
        , ( "body", JE.string article.body )
        , ( "tagList", JE.list [] )
        , ( "createdAt", encodeDate article.createdAt )
        , ( "updatedAt", encodeDate article.updatedAt )
        , ( "favorited", JE.bool False )
        , ( "favoritesCount", JE.int 0 )
        , ( "author", authorProfileObj )
        ]


encodeSingleArticle : Article -> JE.Value -> HandlerState x JE.Value
encodeSingleArticle article authorProfileObj =
    HandlerData <|
        JE.object [ ( "article", encodeArticle article authorProfileObj ) ]


encodeMultipleArticles : List ( Article, JE.Value ) -> HandlerState x JE.Value
encodeMultipleArticles articlesData =
    HandlerData <|
        JE.object
            [ ( "articles"
              , JE.list <|
                    List.map
                        (\( article, authorProfileObj ) -> encodeArticle article authorProfileObj)
                        articlesData
              )
            ]


getArticle : Slug -> EndpointState
getArticle slug =
    let
        article =
            Models.Article.getArticleBySlug slug

        author =
            article
                |> andThen (.author_id >> HandlerData)
                |> andThen Models.User.findById

        isFollowing =
            HandlerData False

        authorProfileObj =
            map2 Models.User.profileObj author isFollowing
    in
        map2 encodeSingleArticle article authorProfileObj
