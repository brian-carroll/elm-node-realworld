module Routes.Profiles exposing (ProfilesRoute, routeParser, dispatch)

-- local imports

import Types exposing (..)
import HandlerState exposing (andThen, onError, tryTask, wrapErrString, map2, map3)
import Routes.Parser exposing (Parser, Method(..), m, (</>), s, string, map, oneOf)
import Routes.Api exposing (requireAuth)
import Models.User
    exposing
        ( User
        , Username(..)
        , profileObj
        , findByUsername
        )


type ProfilesRoute
    = GetProfile Username
    | FollowUser Username
    | UnfollowUser Username


parseUsername : Parser (Username -> state) state
parseUsername =
    map Username string


routeParser : Parser (ProfilesRoute -> state) state
routeParser =
    oneOf
        [ map GetProfile (m GET parseUsername)
        , map FollowUser (m POST (parseUsername </> s "follow"))
        , map UnfollowUser (m DELETE (parseUsername </> s "follow"))
        ]


dispatch : ProgramConfig -> Connection -> ProfilesRoute -> EndpointState
dispatch config conn route =
    case route of
        GetProfile username ->
            getProfile config.secret username conn

        FollowUser username ->
            followProfile config.secret username conn

        UnfollowUser username ->
            unfollowProfile config.secret username conn


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

        writeFollowToDb =
            map2 Models.User.follow
                currentUsername
                (HandlerData profileUsername)
    in
        writeFollowToDb
            |> andThen (\_ -> HandlerData profileUsername)
            |> andThen findByUsername
            |> map2 (flip profileObj) (HandlerData True)


unfollowProfile : Secret -> Username -> Connection -> EndpointState
unfollowProfile secret profileUsername conn =
    let
        currentUsername =
            requireAuth secret conn
                |> andThen (.username >> HandlerData)

        deleteFollowFromDb =
            map2 Models.User.unfollow
                currentUsername
                (HandlerData profileUsername)
    in
        deleteFollowFromDb
            |> andThen (\_ -> HandlerData profileUsername)
            |> andThen findByUsername
            |> map2 (flip profileObj) (HandlerData False)
