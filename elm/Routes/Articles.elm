module Routes.Articles exposing (..)

import UrlParser exposing (Parser, (</>), s, string, map, oneOf, parseString, top)
import Types exposing (..)


type ArticlesRoute
    = AllArticles
    | ArticlesFeed
    | SingleArticle String
    | FavouriteArticle String
    | CommentOnArticle String
    | DeleteArticleComment String String



{-

   get  '/articles/'  auth.optional
   post  '/articles/'  auth.required

   get  '/articles/feed'  auth.required

   get  '/articles/:article'  auth.optional
   put  '/articles/:article'  auth.required
   delete  '/articles/:article'  auth.required

   post  '/articles/:article/favorite'  auth.required
   delete  '/articles/:article/favorite'  auth.required

   post  '/articles/:article/comments'  auth.required
   get  '/articles/:article/comments'  auth.optional
   delete  '/articles/:article/comments/:comment'  auth.required
-}


urlParser : Parser (ArticlesRoute -> parserState) parserState
urlParser =
    oneOf
        [ map AllArticles top
        , map ArticlesFeed (s "feed")
        , map SingleArticle string
        , map FavouriteArticle (string </> s "favourite")
        , map CommentOnArticle (string </> s "comments")
        , map DeleteArticleComment (string </> s "comments" </> string)
        ]
