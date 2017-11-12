module Framework.SqlToElm exposing (..)

{-| Generate Elm code from SQL functions
Relies on certain code conventions
Only tested with PostgreSQL
-}

import Parser exposing (Error)
import Framework.SqlToElm.SqlParser as SqlParser
    exposing
        ( SqlFunctionHeader
        , ArgType(..)
        , ReturnType(..)
        )


convertSqlToElm : String -> Result Error String
convertSqlToElm sql =
    case SqlParser.parseFunctionHeader sql of
        Err e ->
            Err e

        Ok header ->
            header
                |> mapNames
                |> generateElm
                |> Ok


generateElm : ElmFunctionHeader -> String
generateElm header =
    (moduleHeader header.name ++ elmFunction header)
        |> String.join "\n"


moduleHeader : String -> List String
moduleHeader moduleName =
    [ "module " ++ moduleName ++ " exposing (..)"
    , ""
    , "import Json.Decode as JD"
    , "import Json.Encode as JE"
    , ""
    , "encodeMaybe encoder val ="
    , "    Maybe.withDefault JE.null <| Maybe.map encoder val"
    , ""
    ]


capitalise : String -> String
capitalise str =
    let
        first =
            String.left 1 str

        rest =
            String.dropLeft 1 str
    in
        (String.toUpper first) ++ rest


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


type alias ElmFunctionHeader =
    { name : String
    , args : List ( String, ArgType )
    , returnType : String
    }


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


elmFunction : ElmFunctionHeader -> List String
elmFunction { name, args, returnType } =
    let
        argsRecord =
            "{"
                ++ (String.join ", " <| List.map Tuple.first args)
                ++ "}"

        elmFunctionType =
            [ name ++ " : "
            , "    (JD.Decoder " ++ returnType ++ " -> { sql : String, values : List JE.Value } -> m)"
            , "    -> JD.Decoder " ++ returnType
            , "    -> " ++ argsRecord
            , "    -> m"
            ]

        elmFunctionHeader =
            name ++ " runSqlQuery decoder " ++ argsRecord ++ " ="

        sqlFunctionCall =
            "select * from "
                ++ name
                ++ "("
                ++ (dollarArgs args)
                ++ ");"

        values =
            List.map encodeArg args
                |> String.join ("\n" ++ indent 3 ", ")
    in
        elmFunctionType
            ++ [ elmFunctionHeader
               , "    runSqlQuery decoder"
               , "        { sql = \"" ++ sqlFunctionCall ++ "\""
               , "        , values ="
               , "            [ " ++ values
               , "            ]"
               , "        }"
               ]


dollarArgs : List a -> String
dollarArgs argList =
    argList
        |> List.indexedMap (\i _ -> "$" ++ toString i)
        |> String.join ", "


indent : Int -> String -> String
indent n str =
    (String.repeat n "    ") ++ str


encodeArg : ( String, ArgType ) -> String
encodeArg ( argName, argType ) =
    case argType of
        SqlInt ->
            "encodeMaybe JE.int " ++ argName

        SqlText ->
            "encodeMaybe JE.string " ++ argName


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
