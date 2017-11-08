module Tests.Models.Article exposing (..)

import Test exposing (..)
import Expect
import Models.Article exposing (FilterOptions, filter)
import String
import Types exposing (..)
import Diff exposing (Change(..))
import Json.Encode as JE


defaultFilterOptions : FilterOptions
defaultFilterOptions =
    { tag = Nothing
    , author = Nothing
    , favourited = Nothing
    , limit = Nothing
    , offset = Nothing
    , followedBy = Nothing
    }


testCases =
    [ { label = "tag"
      , input = { defaultFilterOptions | tag = Just "mockTag" }
      , sql = """
            select articles.*
            from articles
                inner join article_tags on article_tags.article_id=articles.id
                inner join tags on article_tags.tag_id=tags.id
            where tags.tag=$1
                limit 20 offset 0
            ;
        """
      , values = [ JE.string "mockTag" ]
      }
    , { label = "tag & author"
      , input =
            { defaultFilterOptions
                | tag = Just "mockTag"
                , author = Just "brianc"
            }
      , sql = """
            select articles.*
            from articles
                inner join article_tags on article_tags.article_id=articles.id
                inner join tags on article_tags.tag_id=tags.id
                inner join users as authors on articles.author_id=users.id
            where tags.body=$1
                and authors.username=$2
                limit 20 offset 0
            ;
        """
      , values = [ JE.string "mockTag", JE.string "brianc" ]
      }
    , { label = "tag & author & favourite"
      , input =
            { defaultFilterOptions
                | tag = Just "mockTag"
                , author = Just "brianc"
                , favourited = Just "brianc"
            }
      , sql = """
            select articles.*
            from articles
                inner join article_tags on article_tags.article_id=articles.id
                inner join tags on article_tags.tag_id=tags.id
                inner join users as authors on articles.author_id=users.id
                inner join favourites on favourites.article_id=articles.id
                inner join users as users_favourites on favourites.user_id=users_favourites.id
            where tags.body=$1
                and authors.username=$2
                and users_favourites.username=$3
                limit 20 offset 0
            ;
        """
      , values = [ JE.string "mockTag", JE.string "brianc", JE.string "brianc" ]
      }
    , { label = "feed"
      , input =
            { defaultFilterOptions
                | followedBy = Just "brianc"
            }
      , sql = """
            select articles.*
            from articles
                inner join users as authors on articles.author_id=authors.id
                inner join follows on follows.followed_id=authors.id
                inner join users as follower_users on follows.follower_id=follower_users.id
            where follower_users.username=$1
                limit 20 offset 0
            ;
        """
      , values = [ JE.string "brianc" ]
      }
    , { label = "feed with limit and offset"
      , input =
            { defaultFilterOptions
                | followedBy = Just "brianc"
                , limit = Just 50
                , offset = Just 100
            }
      , sql = """
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
                inner join users as authors on articles.author_id=authors.id
                inner join follows on follows.followed_id=authors.id
                inner join users as follower_users on follows.follower_id=follower_users.id
            where follower_users.username=$1
                limit 50 offset 100
            ;
        """
      , values = [ JE.string "brianc" ]
      }
    ]


normaliseWhitespace : String -> String
normaliseWhitespace str =
    String.words str
        |> String.join " "


filterTestCase testCase =
    test testCase.label <|
        \() ->
            let
                ( generatedSql, values ) =
                    case Models.Article.filter testCase.input of
                        AwaitingPort (SqlQuery { sql, values }) continuation ->
                            ( normaliseWhitespace sql, values )

                        _ ->
                            ( "--Could not match AwaitingPort--", [] )

                expectedSql =
                    normaliseWhitespace testCase.sql
            in
                if generatedSql == expectedSql && values == testCase.values then
                    Expect.pass
                else
                    Diff.diffChars expectedSql generatedSql
                        |> List.map toString
                        |> String.join "\n"
                        |> (\s -> s ++ "\nValues:" ++ toString values)
                        |> Expect.fail


all : Test
all =
    describe "Article"
        [ describe "filter"
            (List.map filterTestCase testCases)
        ]
