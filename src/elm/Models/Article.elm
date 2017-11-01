module Article exposing (..)

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
        (JD.field "createdAt" decodeDate)
        (JD.field "updatedAt" decodeDate)


save : Article -> HandlerState EndpointError Article
save article =
    runSqlQuery (JD.index 0 decodeArticleFromDb)
        { sql =
            case article.id of
                UnsavedArticleId ->
                    """
                    INSERT INTO article(author_id, slug, title, description, body)
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


getArticleBySlug : Slug -> HandlerState EndpointError Article
getArticleBySlug (Slug slug) =
    runSqlQuery (JD.index 0 decodeArticleFromDb)
        { sql = "SELECT * FROM articles WHERE slug=$1 LIMIT 1;"
        , values = [ JE.string slug ]
        }
