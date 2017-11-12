module Framework.SqlToElm exposing (..)

{-| Generate Elm code from SQL functions
Relies on certain code conventions
Only tested with PostgreSQL
-}

import Parser exposing (..)
import Parser.LanguageKit
import Char


type alias SqlFunction =
    { name : String
    , args : List ( String, ArgType )
    , returns : ReturnType
    }


type ArgType
    = SqlInt
    | SqlText


type ReturnType
    = ReturnSetOf String
    | Return String


whitespace : Parser ()
whitespace =
    Parser.LanguageKit.whitespace
        { allowTabs = False -- LanguageKit.whitespace crashes when True!
        , lineComment = Parser.LanguageKit.LineComment "--"
        , multiComment = Parser.LanguageKit.UnnestableComment "/*" "*/"
        }


function : Parser SqlFunction
function =
    succeed SqlFunction
        |. whitespace
        |. functionDeclaration
        |. whitespace
        |= keep oneOrMore Char.isLower
        |. whitespace
        |= argList
        |. whitespace
        |. keyword "returns"
        |. whitespace
        |= returnType


functionDeclaration : Parser ()
functionDeclaration =
    keyword "create"
        |. whitespace
        |. oneOf
            [ (keyword "or"
                |. whitespace
                |. keyword "replace"
                |. whitespace
              )
            , succeed ()
            ]
        |. keyword "function"


returnType : Parser ReturnType
returnType =
    oneOf
        [ map ReturnSetOf <|
            succeed identity
                |. keyword "setof"
                |. whitespace
                |= (map snakeToCamel <|
                        keep oneOrMore
                            Char.isLower
                   )
        , map (Return << snakeToCamel) <|
            keep oneOrMore
                Char.isLower
        ]


argList : Parser (List ( String, ArgType ))
argList =
    Parser.LanguageKit.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = whitespace
        , item = arg
        , trailing = Parser.LanguageKit.Forbidden
        }


arg : Parser ( String, ArgType )
arg =
    succeed (,)
        |. whitespace
        |= argName
        |. whitespace
        |= argType
        |. whitespace
        |. oneOf [ argDefault, succeed () ]


argDefault : Parser ()
argDefault =
    keyword "default"
        |. ignore zeroOrMore (\c -> c /= ',' && c /= ')')


argType : Parser ArgType
argType =
    oneOf
        [ map (\_ -> SqlInt) (keyword "int")
        , map (\_ -> SqlText) (keyword "text")
        ]


argName : Parser String
argName =
    succeed identity
        |. repeat zeroOrMore (symbol "_")
        |= (map snakeToCamel <|
                keep oneOrMore
                    (\c ->
                        c == '_' || Char.isLower c
                    )
           )


snakeToCamel : String -> String
snakeToCamel snake =
    let
        ( camel, _ ) =
            String.foldl snakeToCamelHelp ( "", False ) snake
    in
        camel


snakeToCamelHelp : Char -> ( String, Bool ) -> ( String, Bool )
snakeToCamelHelp c ( camel, capitalizeNext ) =
    if c == '_' then
        ( camel
        , True
        )
    else if capitalizeNext then
        ( camel ++ (String.fromChar <| Char.toUpper c)
        , False
        )
    else
        ( camel ++ (String.fromChar c)
        , False
        )
