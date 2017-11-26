module SqlToElm.ElmCodeGen exposing (generateElm)

import SqlToElm.Types exposing (..)


generateElm : String -> List ElmFunctionHeader -> String
generateElm moduleName functions =
    let
        header =
            [ "module GeneratedCode." ++ moduleName ++ " exposing (..)"
            , ""
            , "import Json.Decode as JD"
            , "import Json.Encode as JE"
            , "import Json.Encode.Extra as JE"
            , "import GeneratedCodeImports exposing (..)"
            ]

        body =
            List.concatMap function functions
    in
        (header ++ body)
            |> String.join "\n"


function : ElmFunctionHeader -> List String
function { sqlName, elmName, args, returnType } =
    let
        argsTypeDecl =
            "{ "
                ++ (String.join ", " <| List.map argTypeDeclaration args)
                ++ " }"

        argsHeader =
            "{ "
                ++ (String.join ", " <| List.map Tuple.first args)
                ++ " }"

        elmFunctionType =
            [ ""
            , ""
            , elmName ++ " : "
            , "    JD.Decoder " ++ returnType
            , "    -> " ++ argsTypeDecl
            , "    -> SqlResult " ++ returnType
            ]

        elmFunctionHeader =
            elmName ++ " decoder " ++ argsHeader ++ " ="

        sqlFunctionCall =
            "select * from "
                ++ sqlName
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
               , ""
               ]


argTypeDeclaration : FunctionArg -> String
argTypeDeclaration ( name, argType ) =
    name
        ++ " : "
        ++ argumentType argType


argumentType : ArgType -> String
argumentType argType =
    case argType of
        SqlInt ->
            "Maybe Int"

        SqlText ->
            "Maybe String"


dollarArgs : List a -> String
dollarArgs argList =
    argList
        |> List.indexedMap (\i _ -> "$" ++ toString (i + 1))
        |> String.join ", "


indent : Int -> String -> String
indent n str =
    (String.repeat n "    ") ++ str


encodeArg : ( String, ArgType ) -> String
encodeArg ( argName, argType ) =
    case argType of
        SqlInt ->
            "JE.maybe JE.int " ++ argName

        SqlText ->
            "JE.maybe JE.string " ++ argName
