module Tests.Models.Article exposing (..)

import Test exposing (..)
import Expect
import Models.Article exposing (FilterOptions, filter)
import String
import Types exposing (..)
import Diff exposing (Change(..))


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
      , output = """
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
                inner join tags on tags.article_id=articles.id
            where tags.body=$1
                limit 20 offset 0
            ;
        """
      }
    , { label = "tag & author"
      , input =
            { defaultFilterOptions
                | tag = Just "stuffTag"
                , author = Just "brianc"
            }
      , output = """
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
                inner join tags on tags.article_id=articles.id
                inner join users as authors on articles.author_id=users.id
            where tags.body=$1
                and authors.username=$2
                limit 20 offset 0
            ;
        """
      }
    , { label = "tag & author & favourite"
      , input =
            { defaultFilterOptions
                | tag = Just "stuffTag"
                , author = Just "brianc"
                , favourited = Just "brianc"
            }
      , output = """
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
                inner join tags on tags.article_id=articles.id
                inner join users as authors on articles.author_id=users.id
                inner join favourites on favourites.article_id=articles.id
                inner join users as users_favourites on favourites.user_id=users_favourites.id
            where tags.body=$1
                and authors.username=$2
                and users_favourites.username=$3
                limit 20 offset 0
            ;
        """
      }
    , { label = "feed"
      , input =
            { defaultFilterOptions
                | followedBy = Just "brianc"
            }
      , output = """
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
                inner join follows on follows.followed_id=authors.id
            where follows.follower_id=$1
                limit 20 offset 0
            ;
        """
      }
    , { label = "feed with limit and offset"
      , input =
            { defaultFilterOptions
                | followedBy = Just "brianc"
                , limit = Just 50
                , offset = Just 100
            }
      , output = """
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
                inner join follows on follows.followed_id=authors.id
            where follows.follower_id=$1
                limit 50 offset 100
            ;
        """
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
                generatedSql =
                    case Models.Article.filter testCase.input of
                        AwaitingPort (SqlQuery { sql, values }) continuation ->
                            normaliseWhitespace sql

                        _ ->
                            "--Could not match AwaitingPort--"

                expectedSql =
                    normaliseWhitespace testCase.output
            in
                if generatedSql == expectedSql then
                    Expect.pass
                else
                    Diff.diffChars expectedSql generatedSql
                        |> List.map toString
                        |> String.join "\n"
                        |> Expect.fail


all : Test
all =
    describe "Article"
        [ describe "filter"
            (List.map filterTestCase testCases)
        ]
