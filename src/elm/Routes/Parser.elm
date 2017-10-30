module Routes.Parser
    exposing
        ( Parser
        , ParseError(..)
        , Method(..)
        , Route
        , string
        , int
        , s
        , m
        , (</>)
        , map
        , oneOf
        , top
        , custom
        , QueryParser
        , (<?>)
        , stringParam
        , intParam
        , customParam
        , parseRoute
        )

import Dict exposing (Dict)
import Http


-- HTTP methods


type Method
    = GET
    | HEAD
    | POST
    | PUT
    | DELETE
    | CONNECT
    | OPTIONS
    | TRACE
    | PATCH


type alias Route =
    { method : String
    , url : String
    }


type ParseError
    = UrlMismatch
    | MethodMismatch
    | BadQueryString



-- PARSERS


{-| Turn method & URL path into nice Elm data
-}
type Parser a b
    = Parser (State a -> List (State b))


type alias State value =
    { method : String
    , visited : List String
    , unvisited : List String
    , params : Dict String String
    , value : value
    }



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.
-}
string : Parser (String -> a) a
string =
    custom "STRING" <|
        \segment ->
            if String.isEmpty segment then
                Err "string does not match empty segment"
            else
                Ok segment


{-| Parse a segment of the path as an `Int`.
-}
int : Parser (Int -> a) a
int =
    custom "NUMBER" String.toInt


{-| Parse a segment of the path if it matches a given string.
-}
s : String -> Parser a a
s str =
    Parser <|
        \{ method, visited, unvisited, params, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    if next == str then
                        [ State method (next :: visited) rest params value ]
                    else
                        []


{-| Modify a parser so that it only matches a particular HTTP method
You should only use this once per endpoint. Only the first usage
will have an effect.

    (parseRoute (m GET top) { method = "GET", path = "/" }) /= Nothing
    --> True

-}
m : Method -> Parser a b -> Parser a b
m requiredMethod (Parser parser) =
    let
        newParser state =
            parser state
                |> List.map
                    (\s -> { s | method = toString requiredMethod })
    in
        Parser newParser


{-| Create a custom path segment parser. Here is how it is used to define the
`int` parser:

    int =
        custom "NUMBER" String.toInt

-}
custom : String -> (String -> Result String a) -> Parser (a -> b) b
custom tipe stringToSomething =
    Parser <|
        \{ method, visited, unvisited, params, value } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    case stringToSomething next of
                        Ok nextValue ->
                            [ State method (next :: visited) rest params (value nextValue) ]

                        Err msg ->
                            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.
-}
(</>) : Parser a b -> Parser b c -> Parser a c
(</>) (Parser parseBefore) (Parser parseAfter) =
    Parser <|
        \state ->
            List.concatMap parseAfter (parseBefore state)
infixr 7 </>


map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parse) =
    Parser <|
        \{ method, visited, unvisited, params, value } ->
            List.map (mapHelp value) <|
                parse <|
                    { method = method
                    , visited = visited
                    , unvisited = unvisited
                    , params = params
                    , value = subValue
                    }


mapHelp : (a -> b) -> State a -> State b
mapHelp func { method, visited, unvisited, params, value } =
    { method = method
    , visited = visited
    , unvisited = unvisited
    , params = params
    , value = func value
    }


oneOf : List (Parser a b) -> Parser a b
oneOf parsers =
    Parser <|
        \state ->
            List.concatMap (\(Parser parser) -> parser state) parsers


top : Parser a a
top =
    Parser <| \state -> [ state ]



-- QUERY PARAMETERS


{-| Turn query parameters like `?name=tom&age=42` into nice Elm data.
-}
type QueryParser a b
    = QueryParser (State a -> List (State b))


{-| Parse some query parameters.
-}
(<?>) : Parser a b -> QueryParser b c -> Parser a c
(<?>) (Parser parser) (QueryParser queryParser) =
    Parser <|
        \state ->
            List.concatMap queryParser (parser state)
infixl 8 <?>


{-| Parse a query parameter as a `String`.
-}
stringParam : String -> QueryParser (Maybe String -> a) a
stringParam name =
    customParam name identity


{-| Parse a query parameter as an `Int`. Maybe you want to show paginated
search results. You could have a `start` query parameter to say which result
should appear first.
-}
intParam : String -> QueryParser (Maybe Int -> a) a
intParam name =
    customParam name intParamHelp


intParamHelp : Maybe String -> Maybe Int
intParamHelp maybeValue =
    case maybeValue of
        Nothing ->
            Nothing

        Just value ->
            Result.toMaybe (String.toInt value)


{-| Create a custom query parser. You could create parsers like these:

    jsonParam : String -> Decoder a -> QueryParser (Maybe a -> b) b

    enumParam : String -> Dict String a -> QueryParser (Maybe a -> b) b

It may be worthwhile to have these in this library directly. If you need
either one in practice, please open an issue [here] describing your exact
scenario. We can use that data to decide if they should be added.

[here]: https://github.com/evancz/url-parser/issues

-}
customParam : String -> (Maybe String -> a) -> QueryParser (a -> b) b
customParam key func =
    QueryParser <|
        \{ method, visited, unvisited, params, value } ->
            [ State
                method
                visited
                unvisited
                params
                (value (func (Dict.get key params)))
            ]



-- RUN A PARSER


parseRoute : Parser (a -> a) a -> Route -> Result ParseError a
parseRoute parser { method, url } =
    case splitPathAndQuery url of
        Nothing ->
            Err BadQueryString

        Just ( pathString, queryParams ) ->
            parse parser { method = method, url = pathString } queryParams



-- PARSER HELPERS


parse : Parser (a -> a) a -> Route -> Dict String String -> Result ParseError a
parse (Parser parser) { method, url } params =
    (parseHelp UrlMismatch method) <|
        parser <|
            { method = ""
            , visited = []
            , unvisited = splitUrl url
            , params = params
            , value = identity
            }


parseHelp : ParseError -> String -> List (State a) -> Result ParseError a
parseHelp leastBadError method states =
    case states of
        [] ->
            Err leastBadError

        state :: rest ->
            let
                methodMatch =
                    state.method == method

                urlMatch =
                    case state.unvisited of
                        [] ->
                            True

                        [ "" ] ->
                            True

                        _ ->
                            False
            in
                if not urlMatch then
                    parseHelp leastBadError method rest
                else if not methodMatch then
                    parseHelp MethodMismatch method rest
                else
                    Ok state.value


splitPathAndQuery : String -> Maybe ( String, Dict String String )
splitPathAndQuery rawPath =
    case String.split "?" rawPath of
        [] ->
            Just ( "", Dict.empty )

        [ path ] ->
            Just ( path, Dict.empty )

        [ path, rawQuery ] ->
            Just ( path, parseParams rawQuery )

        _ ->
            Nothing


splitUrl : String -> List String
splitUrl url =
    case url |> stripLeft "#" |> String.split "/" of
        "" :: segments ->
            segments

        segments ->
            segments


parseParams : String -> Dict String String
parseParams queryString =
    queryString
        |> stripLeft "?"
        |> String.split "&"
        |> List.filterMap toKeyValuePair
        |> Dict.fromList


stripLeft : String -> String -> String
stripLeft pattern string =
    if string |> String.startsWith pattern then
        String.dropLeft (String.length pattern) string
    else
        string


toKeyValuePair : String -> Maybe ( String, String )
toKeyValuePair segment =
    case String.split "=" segment of
        [ key, value ] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

        _ ->
            Nothing
