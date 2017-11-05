module Models.Article exposing (..)

-- external imports

import Date exposing (Date)
import Json.Decode as JD
import Json.Encode as JE


-- local imports

import Models.User exposing (UserId, encodeUserId)
import Models.Utils exposing (runSqlQuery)
import Types exposing (..)


type Slug
    = Slug String


type ArticleId
    = ArticleId Int
    | UnsavedArticleId


type alias Article =
    { id : ArticleId
    , author_id : UserId
    , slug : Slug
    , title : String
    , description : String
    , body : String
    , createdAt : Date
    , updatedAt : Date
    }


encodeSlug : Slug -> JE.Value
encodeSlug (Slug slug) =
    JE.string slug


encodeArticleForDb : Article -> List JE.Value
encodeArticleForDb article =
    let
        idAsList =
            case article.id of
                UnsavedArticleId ->
                    []

                ArticleId id ->
                    [ JE.int id ]
    in
        idAsList
            ++ encodeUserId article.author_id
            ++ [ encodeSlug article.slug
               , JE.string article.title
               , JE.string article.description
               , JE.string article.body

               -- deliberately leave out the dates, let the DB do it
               ]


decodeDate : JD.Decoder Date
decodeDate =
    JD.string
        |> JD.andThen
            (\s ->
                case Date.fromString s of
                    Ok d ->
                        JD.succeed d

                    Err e ->
                        JD.fail e
            )


decodeArticleFromDb : JD.Decoder Article
decodeArticleFromDb =
    JD.map8 Article
        (JD.field "id" (JD.map ArticleId JD.int))
        (JD.field "author_id" (JD.map Models.User.UserId JD.int))
        (JD.field "slug" (JD.map Slug JD.string))
        (JD.field "title" JD.string)
        (JD.field "description" JD.string)
        (JD.field "body" JD.string)
        (JD.field "created_at" decodeDate)
        (JD.field "updated_at" decodeDate)


save : Article -> HandlerState EndpointError Article
save article =
    runSqlQuery (JD.index 0 decodeArticleFromDb)
        { sql =
            case article.id of
                UnsavedArticleId ->
                    """
                    INSERT INTO articles(author_id, slug, title, description, body)
                    VALUES($1,$2,$3,$4,$5,$6) RETURNING *;
                    """

                ArticleId _ ->
                    """
                    UPDATE users SET
                    author_id=$2, slug=$3, title=$4, description=$5, body=$6
                    WHERE id=$1 RETURNING *;
                    """
        , values = encodeArticleForDb article
        }


getArticles : HandlerState EndpointError (List Article)
getArticles =
    runSqlQuery (JD.list decodeArticleFromDb)
        { sql = "SELECT * FROM articles;"
        , values = []
        }


sqlSelectArticle : String
sqlSelectArticle =
    """
    select
        articles.id,
        articles.author_id,
        articles.slug,
        articles.title,
        articles.description,
        articles.body,
        to_char(articles.created_at, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"') as created_at,
        to_char(articles.updated_at, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"') as updated_at
    from articles
    """


getArticleBySlug : Slug -> HandlerState EndpointError Article
getArticleBySlug (Slug slug) =
    runSqlQuery (JD.index 0 decodeArticleFromDb)
        { sql = sqlSelectArticle ++ "where slug=$1;"
        , values = [ JE.string slug ]
        }


type alias FilterOptions =
    { tag : Maybe String
    , author : Maybe String
    , favourited : Maybe String
    , followedBy : Maybe String
    , limit : Maybe Int
    , offset : Maybe Int
    }


type alias SqlSnippet =
    { value : Maybe JE.Value
    , joinClause : String
    , whereClause : String
    }


{-| Filter Articles from the database, given a set of filter options
-}
filter : FilterOptions -> HandlerState EndpointError (List Article)
filter filterOptions =
    let
        snippets =
            [ { value = Maybe.map JE.string filterOptions.tag
              , joinClause = "inner join tags on tags.article_id=articles.id"
              , whereClause = "tags.body="
              }
            , { value = Maybe.map JE.string filterOptions.author
              , joinClause = "inner join users as authors on articles.author_id=users.id"
              , whereClause = "authors.username="
              }
            , { value = Maybe.map JE.string filterOptions.favourited
              , joinClause = """
                    inner join favourites on favourites.article_id=articles.id
                    inner join users as users_favourites on favourites.user_id=users_favourites.id
                """
              , whereClause = "users_favourites.username="
              }
            , { value = Maybe.map JE.string filterOptions.followedBy
              , joinClause = "inner join follows on follows.followed_id=authors.id"
              , whereClause = "follows.follower_id="
              }
            ]

        ( joinClause, whereClauses, jsValues ) =
            filterHelp snippets "" [] []

        whereClause =
            case whereClauses of
                [] ->
                    ""

                _ ->
                    " where " ++ (String.join " and " whereClauses)

        sql =
            sqlSelectArticle
                ++ joinClause
                ++ whereClause
                ++ (" limit " ++ (toString <| Maybe.withDefault 20 filterOptions.limit))
                ++ (" offset " ++ (toString <| Maybe.withDefault 0 filterOptions.offset))
                ++ " ;"
    in
        runSqlQuery
            (JD.list decodeArticleFromDb)
            { sql = sql
            , values = jsValues
            }


filterHelp : List SqlSnippet -> String -> List String -> List JE.Value -> ( String, List String, List JE.Value )
filterHelp sqlSnippets joinClause whereClauses jsValues =
    case sqlSnippets of
        [] ->
            ( joinClause, whereClauses, jsValues )

        sqlSnippet :: rest ->
            case sqlSnippet.value of
                Nothing ->
                    filterHelp rest joinClause whereClauses jsValues

                Just jsValue ->
                    filterHelp
                        rest
                        (joinClause ++ " " ++ sqlSnippet.joinClause ++ " ")
                        (whereClauses
                            ++ [ sqlSnippet.whereClause
                                    ++ "$"
                                    ++ (toString (1 + List.length jsValues))
                                    ++ " "
                               ]
                        )
                        (jsValues ++ [ jsValue ])
