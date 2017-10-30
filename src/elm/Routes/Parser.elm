module Routes.Parser
    exposing
        ( RouteParser
        , Parser
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



-- PARSERS


{-| Turn method & URL path into nice Elm data
-}
type alias RouteParser a b =
    Parser { method : String } a b


type alias State value =
    { method : String
    , visited : List String
    , unvisited : List String
    , params : Dict String String
    , value : value
    }


type Parser m a b
    = Parser (UrlState m a -> List (UrlState m b))


type alias UrlState m value =
    { m
        | visited : List String
        , unvisited : List String
        , params : Dict String String
        , value : value
    }



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.
-}
string : Parser m (String -> a) a
string =
    custom "STRING" <|
        \segment ->
            if String.isEmpty segment then
                Err "string does not match empty segment"
            else
                Ok segment


{-| Parse a segment of the path as an `Int`.
-}
int : Parser m (Int -> a) a
int =
    custom "NUMBER" String.toInt


{-| Parse a segment of the path if it matches a given string.
-}
s : String -> Parser m a a
s str =
    Parser <|
        \state ->
            case state.unvisited of
                [] ->
                    []

                next :: rest ->
                    if next == str then
                        [ { state | visited = (next :: state.visited), unvisited = rest } ]
                    else
                        []


{-| Modify a parser so that it only matches a particular HTTP method
You should only use this once per endpoint. Only the first usage
will have an effect.

    (parseRoute (m GET top) { method = "GET", path = "/" }) /= Nothing
    --> True

-}
m : Method -> RouteParser a b -> RouteParser a b
m requiredMethod (Parser parser) =
    let
        f : State a -> List (State b)
        f state =
            if state.method /= toString requiredMethod then
                []
            else
                parser state
    in
        Parser f


{-| Create a custom path segment parser. Here is how it is used to define the
`int` parser:

    int =
        custom "NUMBER" String.toInt

-}
custom : String -> (String -> Result String a) -> Parser m (a -> b) b
custom tipe stringToSomething =
    Parser <|
        \state ->
            case state.unvisited of
                [] ->
                    []

                next :: rest ->
                    case stringToSomething next of
                        Ok nextValue ->
                            [ { state | visited = (next :: state.visited), unvisited = rest, value = (state.value nextValue) } ]

                        Err msg ->
                            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.
-}
(</>) : Parser m a b -> Parser m b c -> Parser m a c
(</>) (Parser parseBefore) (Parser parseAfter) =
    Parser <|
        \state ->
            List.concatMap parseAfter (parseBefore state)
infixr 7 </>


map : a -> Parser m a b -> Parser m (b -> c) c
map subValue (Parser parse) =
    Parser <|
        \state ->
            List.map (mapHelp state.value) <|
                parse <|
                    { state
                        | value = subValue
                    }


mapHelp : (a -> b) -> UrlState m a -> UrlState m b
mapHelp func state =
    { state
        | value = func state.value
    }


oneOf : List (Parser m a b) -> Parser m a b
oneOf parsers =
    Parser <|
        \state ->
            List.concatMap (\(Parser parser) -> parser state) parsers


top : Parser m a a
top =
    Parser <| \state -> [ state ]



-- QUERY PARAMETERS


{-| Turn query parameters like `?name=tom&age=42` into nice Elm data.
-}
type QueryParser m a b
    = QueryParser (UrlState m a -> List (UrlState m b))


{-| Parse some query parameters.
-}
(<?>) : Parser m a b -> QueryParser m b c -> Parser m a c
(<?>) (Parser parser) (QueryParser queryParser) =
    Parser <|
        \state ->
            List.concatMap queryParser (parser state)
infixl 8 <?>


{-| Parse a query parameter as a `String`.
-}
stringParam : String -> QueryParser m (Maybe String -> a) a
stringParam name =
    customParam name identity


{-| Parse a query parameter as an `Int`. Maybe you want to show paginated
search results. You could have a `start` query parameter to say which result
should appear first.
-}
intParam : String -> QueryParser m (Maybe Int -> a) a
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
customParam : String -> (Maybe String -> a) -> QueryParser m (a -> b) b
customParam key func =
    QueryParser <|
        \state ->
            [ { state | value = (state.value (func (Dict.get key state.params))) } ]



-- RUN A PARSER


parseRoute : RouteParser (a -> a) a -> Route -> Maybe a
parseRoute parser { method, url } =
    splitPathAndQuery url
        |> Maybe.andThen
            (\( pathString, queryParams ) ->
                parse parser { method = method, url = pathString } queryParams
            )



-- PARSER HELPERS


parse : RouteParser (a -> a) a -> Route -> Dict String String -> Maybe a
parse (Parser parser) { method, url } params =
    parseHelp <|
        parser <|
            { method = method
            , visited = []
            , unvisited = splitUrl url
            , params = params
            , value = identity
            }


parseHelp : List (State a) -> Maybe a
parseHelp states =
    case states of
        [] ->
            Nothing

        state :: rest ->
            case state.unvisited of
                [] ->
                    Just state.value

                [ "" ] ->
                    Just state.value

                _ ->
                    parseHelp rest


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
