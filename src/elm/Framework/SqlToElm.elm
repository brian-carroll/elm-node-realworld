module Framework.SqlToElm exposing (..)

{-| Generate Elm code from SQL functions
Relies on certain code conventions
Only tested with PostgreSQL
-}

import Parser exposing (Error)
import Framework.SqlToElm.Types exposing (..)
import Framework.SqlToElm.SqlParser exposing (parseSqlFunctionHeader)
import Framework.SqlToElm.ElmCodeGen exposing (generateElm)


convertSqlToElm : String -> Result Error String
convertSqlToElm sql =
    case parseSqlFunctionHeader sql of
        Err e ->
            Err e

        Ok header ->
            header
                |> mapNames
                |> generateElm
                |> Ok


mapNames : SqlFunctionHeader -> ElmFunctionHeader
mapNames sqlHeader =
    { name = snakeToCamel sqlHeader.name
    , args =
        List.map
            (\( argName, argType ) ->
                ( snakeToCamel argName
                , argType
                )
            )
            sqlHeader.args
    , returnType =
        case sqlHeader.returnType of
            ReturnSetOf sqlTypeName ->
                "(List " ++ (mapReturnType sqlTypeName) ++ ")"

            Return sqlTypeName ->
                mapReturnType sqlTypeName
    }


mapReturnType : String -> String
mapReturnType tableName =
    let
        singular =
            if String.endsWith "s" tableName then
                String.dropRight 1 tableName
            else
                tableName
    in
        capitalise singular


snakeToCamel : String -> String
snakeToCamel snake =
    snake
        |> String.split "_"
        |> List.filter (not << String.isEmpty)
        |> List.indexedMap
            (\i s ->
                if i == 0 then
                    s
                else
                    capitalise s
            )
        |> String.join ""


capitalise : String -> String
capitalise str =
    let
        first =
            String.left 1 str

        rest =
            String.dropLeft 1 str
    in
        (String.toUpper first) ++ rest
