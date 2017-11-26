module Routes.Articles exposing (ArticlesRoute, routeParser, dispatch)

-- external imports

import Json.Decode as JD
import Json.Encode as JE
import Date


-- local imports

import Framework.RouteParser
    exposing
        ( Parser
        , QueryParser
        , Method(..)
        , m
        , (</>)
        , s
        , string
        , map
        , oneOf
        , parseRoute
        , top
        , stringParam
        , intParam
        , (<?>)
        )
import Types exposing (..)
import Models.User exposing (User, UserId(..), Username(..))
import Models.Article exposing (Article, Slug(..), ArticleId(..), FilterOptions)
import Framework.HandlerState as HS exposing (andThen, andThen2, wrapErrString)
import Routes.Api exposing (encodeDate)
import GeneratedCode.Articles


type CommentId
    = CommentId String


type alias ListOptions =
    { tag : Maybe String
    , author : Maybe String
    , favourited : Maybe String
    , limit : Maybe Int
    , offset : Maybe Int
    }


type alias FeedOptions =
    { limit : Maybe Int
    , offset : Maybe Int
    }


type ArticlesRoute
    = ListArticles ListOptions
    | FeedArticles FeedOptions
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


listOptionsParser : Parser (ListOptions -> state) state
listOptionsParser =
    map ListOptions <|
        top
            <?> stringParam "tag"
            <?> stringParam "author"
            <?> stringParam "favourited"
            <?> intParam "limit"
            <?> intParam "offset"


feedOptionsParser : Parser (FeedOptions -> state) state
feedOptionsParser =
    map FeedOptions <|
        top
            <?> intParam "limit"
            <?> intParam "offset"


routeParser : Parser (ArticlesRoute -> state) state
routeParser =
    oneOf
        [ map ListArticles (m GET listOptionsParser)
        , map FeedArticles (m GET (s "feed" </> feedOptionsParser))
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


dispatch : HandlerState EndpointError Username -> Connection -> ArticlesRoute -> EndpointState
dispatch authUsername conn route =
    case route of
        ListArticles filterOptions ->
            listArticles filterOptions conn

        FeedArticles filterOptions ->
            HandlerData JE.null

        GetArticle slug ->
            getArticle authUsername slug

        CreateArticle ->
            createArticle authUsername conn

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


createArticle : HandlerState EndpointError Username -> Connection -> EndpointState
createArticle authUsername conn =
    let
        formData : HandlerState EndpointError CreateArticleForm
        formData =
            (HandlerData <|
                JD.decodeString
                    (JD.field "article" <|
                        JD.map4 CreateArticleForm
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
                , slug = Models.Article.titleToSlug formData.title
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


listArticles : ListOptions -> Connection -> EndpointState
listArticles options conn =
    let
        sqlQuery : HandlerState EndpointError (List Article)
        sqlQuery =
            GeneratedCode.Articles.listArticles
                (JD.list Models.Article.decodeArticleFromDb)
                options
    in
        sqlQuery
            |> HS.map (List.map (\a -> ( a, JE.null )))
            |> andThen encodeMultipleArticles
