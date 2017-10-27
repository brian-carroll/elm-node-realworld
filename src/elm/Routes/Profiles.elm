module Routes.Profiles exposing (..)

-- library imports

import Json.Encode as JE
import UrlParser exposing (Parser, (</>), s, string, map, oneOf, parseString, top)


-- local imports

import Types exposing (..)
import HandlerState exposing (andThen, onError, tryTask, wrapErrString, map2, map3)
import Routes.Api exposing (requireAuth)
import Models.User
    exposing
        ( User
        , Username(..)
        , profileObj
        , findByUsername
        )


type ProfilesRoute
    = ShowProfile Username
    | FollowProfile Username


urlParser : Parser (ProfilesRoute -> parserState) parserState
urlParser =
    oneOf
        [ map (ShowProfile << Username) string
        , map (FollowProfile << Username) (string </> s "follow")
        ]


dispatch : ProgramConfig -> Connection -> ProfilesRoute -> EndpointState
dispatch config conn route =
    let
        method =
            conn.request.method

        methodNotAllowedError =
            HandlerError { status = MethodNotAllowed, messages = [] }
    in
        case route of
            ShowProfile username ->
                case method of
                    Get ->
                        getProfile config.secret username conn

                    _ ->
                        methodNotAllowedError

            FollowProfile username ->
                case method of
                    Post ->
                        wrapErrString NotFound "Not implemented"

                    Delete ->
                        wrapErrString NotFound "Not implemented"

                    _ ->
                        methodNotAllowedError


getProfile : Secret -> Username -> Connection -> EndpointState
getProfile secret profileUsername conn =
    let
        currentUsername =
            requireAuth secret conn
                |> andThen (.username >> HandlerData)

        profileUser =
            HandlerData profileUsername
                |> andThen findByUsername

        isFollowing =
            case currentUsername of
                HandlerError _ ->
                    HandlerData False

                _ ->
                    map2 Models.User.isFollowing
                        currentUsername
                        (HandlerData profileUsername)
    in
        map2 profileObj profileUser isFollowing


followProfile : Secret -> Username -> Connection -> EndpointState
followProfile secret profileUsername conn =
    let
        currentUsername =
            requireAuth secret conn
                |> andThen (.username >> HandlerData)
    in
        HandlerData JE.null
