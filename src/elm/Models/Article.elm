module Article exposing (..)

import Date exposing (Date)
import Json.Decode as JD
import Json.Encode as JE
import Models.User exposing (UserId, encodeUserId)


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
encodeSlug slug =
    case slug of
        Slug str ->
            JE.string str


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
               ]



-- decodeArticleFromDb : JD.Decoder Article
-- decodeArticleFromDb =
{-

   create table if not exists articles
       ( id serial primary key
       , author_id int not null references users
       , slug text not null unique
       , title text not null
       , description text not null
       , body text not null
       , created_at timestamptz default now()
       , updated_at timestamptz default now()
       );

    select to_char(current_timestamp, 'yyyy-mm-ddThh24:mi:ss.msZ')
-}
