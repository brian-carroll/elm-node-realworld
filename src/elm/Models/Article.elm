module Models.Article exposing (..)

-- external imports

import Date exposing (Date)
import Json.Decode as JD
import Json.Encode as JE


-- local imports

import Models.User exposing (UserId, Username(..), encodeUserId)
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


titleToSlug : String -> Slug
titleToSlug title =
    title
        |> String.toLower
        |> String.words
        |> String.join "-"
        |> Slug


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


decodeFullArticleFromDb : JD.Decoder Article
decodeFullArticleFromDb =
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
                    VALUES($1,$2,$3,$4,$5) RETURNING *;
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


getArticleBySlug : Slug -> HandlerState EndpointError Article
getArticleBySlug (Slug slug) =
    runSqlQuery (JD.index 0 decodeArticleFromDb)
        { sql = "select * from articles where slug=$1;"
        , values = [ JE.string slug ]
        }


getFullArticleBySlug : Slug -> Int -> HandlerState EndpointError Article
getFullArticleBySlug (Slug slug) userId =
    runSqlQuery (JD.index 0 decodeFullArticleFromDb)
        { sql = """
            select
                articles.*,
                authors.*,
                count(favourites) as favourites_count,
                sum(case when favourites.user_id=5 then 1 else 0 end)>0 as favourited,
                sum(case when follows.follower_id=5 then 1 else 0 end)>0 as following_author,
                array(
                    select tags.tag from
                    article_tags inner join tags on article_tags.tag_id=tags.id
                    where article_tags.article_id=articles.id
                ) as tag_list
            from articles
                inner join users as authors on articles.author_id=authors.id
                left join favourites on favourites.article_id=articles.id
                left join follows on follows.followed_id=authors.id
            group by
                articles.id, authors.id
            order by
                articles.created_at desc
            ;"""
        , values = [ JE.int userId, JE.string slug ]
        }



{-
   where
       slug=$2

    select
        articles.*,
        authors.*,
        count(favourites) as favourites_count,
        sum(case when favourites.user_id=5 then 1 else 0 end)>0 as favourited,
        sum(case when follows.follower_id=5 then 1 else 0 end)>0 as following_author
    from articles
        inner join users as authors on articles.author_id=authors.id
        left join favourites on favourites.article_id=articles.id
        left join follows on follows.followed_id=authors.id
    group by
        articles.id, authors.id
    order by
        articles.created_at desc

-}


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
            [ { joinClause = """
                    inner join article_tags on article_tags.article_id=articles.id
                    inner join tags on article_tags.tag_id=tags.id
                """
              , whereClause = "tags.body="
              , value = Maybe.map JE.string filterOptions.tag
              }
            , { joinClause = "inner join users as authors on articles.author_id=users.id"
              , whereClause = "authors.username="
              , value = Maybe.map JE.string filterOptions.author
              }
            , { joinClause = """
                    inner join favourites on favourites.article_id=articles.id
                    inner join users as users_favourites on favourites.user_id=users_favourites.id
                """
              , whereClause = "users_favourites.username="
              , value = Maybe.map JE.string filterOptions.favourited
              }
            , { joinClause = """
                    inner join users as authors on articles.author_id=authors.id
                    inner join follows on follows.followed_id=authors.id
                    inner join users as follower_users on follows.follower_id=follower_users.id
                """
              , whereClause = "follower_users.username="
              , value = Maybe.map JE.string filterOptions.followedBy
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
            "select articles.* from articles "
                ++ joinClause
                ++ whereClause
                ++ (" limit " ++ (toString <| Maybe.withDefault 20 filterOptions.limit))
                ++ (" offset " ++ (toString <| Maybe.withDefault 0 filterOptions.offset))
                ++ " order by articles.created_at desc;"
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
