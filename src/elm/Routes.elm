module Routes exposing (..)

-- external imports

import Json.Encode as JE
import Dict


-- local imports

import Types exposing (..)
import Routes.Parser exposing (Parser, ParseError(..), Route, oneOf, map, s, (</>), parseRoute)
import Routes.Users exposing (UsersRoute, urlParserUsers, urlParserUser)
import Routes.Profiles exposing (ProfilesRoute)
import Routes.Articles exposing (ArticlesRoute)
import Routes.Tags exposing (TagsRoute)
import Models.User exposing (Username, JwtPayload, verifyJWT)
import HandlerState exposing (wrapErrString, andThen, onError)


type Route
    = Tags TagsRoute
    | Profiles ProfilesRoute
    | Articles ArticlesRoute
    | Users UsersRoute


routeParser : Parser (Route -> parserState) parserState
routeParser =
    s "api"
        </> oneOf
                [ map Tags (s "tags" </> Routes.Tags.routeParser)
                , map Profiles (s "profiles" </> Routes.Profiles.routeParser)
                , map Articles (s "articles" </> Routes.Articles.routeParser)
                , map Users (s "users" </> urlParserUsers)
                , map Users (s "user" </> urlParserUser)
                ]


parseAuthToken : Secret -> Connection -> Result String JwtPayload
parseAuthToken secret conn =
    case Dict.get "authorization" conn.request.headers of
        Just authHeader ->
            case String.words authHeader of
                [ "Token", token ] ->
                    verifyJWT secret conn.timestamp token

                _ ->
                    Err "Invalid auth token"

        Nothing ->
            Err "Missing Authorization header"


extractAuthUsername : Secret -> Connection -> HandlerState EndpointError Username
extractAuthUsername secret conn =
    parseAuthToken secret conn
        |> Result.map .username
        |> HandlerData
        |> onError (wrapErrString Unauthorized)


dispatch : ProgramConfig -> Connection -> EndpointState
dispatch config conn =
    let
        route =
            Debug.log "Route" <|
                parseRoute
                    routeParser
                    (Route conn.request.method conn.request.url)

        authUsername =
            extractAuthUsername config.secret conn
    in
        case route of
            Ok subroute ->
                case subroute of
                    Tags tagsRoute ->
                        HandlerData JE.null

                    Profiles profilesRoute ->
                        Routes.Profiles.dispatch authUsername conn profilesRoute

                    Articles articlesRoute ->
                        Routes.Articles.dispatch authUsername conn articlesRoute

                    Users usersRoute ->
                        Routes.Users.dispatch config.secret authUsername conn usersRoute

            Err parseError ->
                case parseError of
                    UrlMismatch ->
                        HandlerError { status = NotFound, messages = [] }

                    MethodMismatch ->
                        HandlerError { status = MethodNotAllowed, messages = [] }

                    BadQueryString ->
                        HandlerError { status = BadRequest, messages = [] }
