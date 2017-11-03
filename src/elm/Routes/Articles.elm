module Routes.Articles exposing (ArticlesRoute, routeParser, dispatch)

-- external imports

import Json.Decode as JD
import Json.Encode as JE
import Date


-- local imports

import Routes.Parser exposing (Parser, Method(..), m, (</>), s, string, map, oneOf, parseRoute, top)
import Types exposing (..)
import Models.User exposing (User, UserId(..), Username(..))
import Models.Article exposing (Article, Slug(..), ArticleId(..))
import HandlerState as HS exposing (andThen, andThen2, wrapErrString)
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


dispatch : HandlerState EndpointError Username -> ArticlesRoute -> EndpointState
dispatch authUsername route =
    case route of
        ListArticles ->
            HandlerData JE.null

        FeedArticles ->
            HandlerData JE.null

        GetArticle slug ->
            getArticle authUsername slug

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


getArticle : HandlerState EndpointError Username -> Slug -> EndpointState
getArticle authUsername slug =
    let
        article =
            Models.Article.getArticleBySlug slug

        author =
            article
                |> HS.map .author_id
                |> andThen Models.User.findById

        isFollowing =
            case authUsername of
                HandlerError _ ->
                    HandlerData False

                _ ->
                    author
                        |> HS.map .username
                        |> andThen2 Models.User.isFollowing authUsername

        authorProfileObj =
            andThen2 Models.User.profileObj author isFollowing
    in
        andThen2 encodeSingleArticle article authorProfileObj


type alias CreateArticleForm =
    { title : String
    , description : String
    , body : String
    , tagList : List String
    }


createArticle : HandlerState EndpointError Username -> Slug -> Connection -> EndpointState
createArticle authUsername slug conn =
    let
        formData : HandlerState EndpointError CreateArticleForm
        formData =
            (HandlerData <|
                JD.decodeString
                    (JD.map4 CreateArticleForm
                        (JD.field "title" JD.string)
                        (JD.field "description" JD.string)
                        (JD.field "body" JD.string)
                        (JD.field "tagList" (JD.list JD.string))
                    )
                    conn.request.body
            )
                |> HS.onError (wrapErrString UnprocessableEntity)

        -- not handling tagList from form yet
        buildArticle : CreateArticleForm -> UserId -> Article
        buildArticle formData authorId =
            let
                dummyDateForDbToOverwrite =
                    Date.fromTime 0
            in
                { id = UnsavedArticleId
                , author_id = authorId
                , slug = slug
                , title = formData.title
                , description = formData.description
                , body = formData.body
                , createdAt = dummyDateForDbToOverwrite
                , updatedAt = dummyDateForDbToOverwrite
                }

        author : HandlerState EndpointError User
        author =
            authUsername
                |> andThen Models.User.findByUsername

        savedArticle : HandlerState EndpointError Article
        savedArticle =
            author
                |> HS.map .id
                |> HS.map2 buildArticle formData
                |> HS.andThen Models.Article.save

        -- TODO: save Tags with ArticleId & put them in encodeSingleArticle
        {- -}
        authorProfileObj =
            HandlerData False
                |> andThen2 Models.User.profileObj author
    in
        authorProfileObj
            |> andThen2 encodeSingleArticle savedArticle
