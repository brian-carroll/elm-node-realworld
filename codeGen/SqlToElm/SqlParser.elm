module SqlToElm.SqlParser exposing (parseSqlFunctions)

{-| Parse header section of SQL 'create function' statements
Only tested with PostgreSQL
-}

import Char
import Parser.LanguageKit
import SqlToElm.Types exposing (..)
import Parser
    exposing
        ( Parser
        , Error
        , run
        , succeed
        , keep
        , ignore
        , oneOf
        , oneOrMore
        , zeroOrMore
        , keyword
        , symbol
        , ignoreUntil
        , repeat
        , (|=)
        , (|.)
        , map
        )


parseSqlFunctions : String -> Result Error (List SqlFunction)
parseSqlFunctions sql =
    run functionList sql


functionList : Parser (List SqlFunction)
functionList =
    repeat oneOrMore function
        |. whitespace


function : Parser SqlFunction
function =
    succeed SqlFunction
        |. whitespace
        |. functionDeclaration
        |. whitespace
        |= keep oneOrMore (\c -> Char.isLower c || c == '_')
        |. whitespace
        |= argList
        |. whitespace
        |. keyword "returns"
        |. whitespace
        |= returnType
        |. whitespace
        |. symbol "as"
        |. whitespace
        |. functionBody
        |. whitespace
        |. functionLanguage
        |. whitespace


whitespace : Parser ()
whitespace =
    Parser.LanguageKit.whitespace
        { allowTabs = False -- LanguageKit.whitespace crashes when True!
        , lineComment = Parser.LanguageKit.LineComment "--"
        , multiComment = Parser.LanguageKit.UnnestableComment "/*" "*/"
        }


functionBody : Parser ()
functionBody =
    symbol "$$"
        |. ignoreUntil "$$"


functionLanguage : Parser ()
functionLanguage =
    symbol "language"
        |. ignoreUntil ";"


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
                |= keep oneOrMore
                    Char.isLower
        , map Return <|
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
        |= keep oneOrMore
            (\c ->
                c == '_' || Char.isLower c
            )
