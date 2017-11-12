module Framework.SqlToElm.ElmCodeGen exposing (generateElm)

import Framework.SqlToElm.Types exposing (..)


generateElm : ElmFunctionHeader -> String
generateElm header =
    (moduleHeader header.name ++ function header)
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


function : ElmFunctionHeader -> List String
function { name, args, returnType } =
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
