module Framework.SqlToElm.SqlParser
    exposing
        ( SqlFunctionHeader
        , ArgType(..)
        , ReturnType(..)
        , parseFunctionHeader
        )

{-| Parse header section of SQL 'create function' statements
Only tested with PostgreSQL
-}

import Char
import Parser.LanguageKit
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
        , (|=)
        , (|.)
        , map
        )


type alias SqlFunctionHeader =
    { name : String
    , args : List ( String, ArgType )
    , returnType : ReturnType
    }


type ArgType
    = SqlInt
    | SqlText


type ReturnType
    = ReturnSetOf String
    | Return String


parseFunctionHeader : String -> Result Error SqlFunctionHeader
parseFunctionHeader sql =
    run functionHeader sql


functionHeader : Parser SqlFunctionHeader
functionHeader =
    succeed SqlFunctionHeader
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


whitespace : Parser ()
whitespace =
    Parser.LanguageKit.whitespace
        { allowTabs = False -- LanguageKit.whitespace crashes when True!
        , lineComment = Parser.LanguageKit.LineComment "--"
        , multiComment = Parser.LanguageKit.UnnestableComment "/*" "*/"
        }


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
