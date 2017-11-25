module SqlToElm exposing (convert)

{-| Generate Elm code from SQL functions
-}

import Parser exposing (Error)
import SqlToElm.Types exposing (..)
import SqlToElm.SqlParser exposing (parseSqlFunctions)
import SqlToElm.ElmCodeGen exposing (generateElm)


convert : { a | elmDir : String } -> File -> Result Error File
convert config { path, body } =
    case parseSqlFunctions body of
        Err e ->
            Err e

        Ok sqlFunctions ->
            let
                elmFunctionHeaders =
                    List.map
                        mapNames
                        sqlFunctions

                elmModuleName =
                    path
                        |> String.split "/"
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.split "."
                        |> List.head
                        |> Maybe.withDefault ""
                        |> capitalise
            in
                Ok
                    { path = config.elmDir ++ "/" ++ elmModuleName ++ ".elm"
                    , body = generateElm elmModuleName elmFunctionHeaders
                    }


mapNames : SqlFunction -> ElmFunctionHeader
mapNames sqlFunction =
    { sqlName = sqlFunction.name
    , elmName = snakeToCamel sqlFunction.name
    , args =
        List.map
            (\( argName, argType ) ->
                ( snakeToCamel argName
                , argType
                )
            )
            sqlFunction.args
    , returnType =
        case sqlFunction.returnType of
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
