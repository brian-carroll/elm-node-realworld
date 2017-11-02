module Routes.Profiles exposing (ProfilesRoute, routeParser, dispatch)

-- local imports

import Types exposing (..)
import HandlerState exposing (andThen, onError, tryTask, wrapErrString, map2, map3)
import Routes.Parser exposing (Parser, Method(..), m, (</>), s, string, map, oneOf)
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


dispatch : HandlerState EndpointError Username -> Connection -> ProfilesRoute -> EndpointState
dispatch authUsername conn route =
    case route of
        GetProfile profileUsername ->
            getProfile authUsername profileUsername conn

        FollowUser profileUsername ->
            followProfile authUsername profileUsername conn

        UnfollowUser profileUsername ->
            unfollowProfile authUsername profileUsername conn


getProfile : HandlerState EndpointError Username -> Username -> Connection -> EndpointState
getProfile authUsername profileUsername conn =
    let
        profileUser =
            HandlerData profileUsername
                |> andThen findByUsername

        isFollowing =
            case authUsername of
                HandlerError _ ->
                    HandlerData False

                _ ->
                    map2 Models.User.isFollowing
                        authUsername
                        (HandlerData profileUsername)
    in
        map2 profileObj profileUser isFollowing


followProfile : HandlerState EndpointError Username -> Username -> Connection -> EndpointState
followProfile authUsername profileUsername conn =
    let
        writeFollowToDb =
            map2 Models.User.follow
                authUsername
                (HandlerData profileUsername)
    in
        writeFollowToDb
            |> andThen (\_ -> HandlerData profileUsername)
            |> andThen findByUsername
            |> map2 (flip profileObj) (HandlerData True)


unfollowProfile : HandlerState EndpointError Username -> Username -> Connection -> EndpointState
unfollowProfile authUsername profileUsername conn =
    let
        deleteFollowFromDb =
            map2 Models.User.unfollow
                authUsername
                (HandlerData profileUsername)
    in
        deleteFollowFromDb
            |> andThen (\_ -> HandlerData profileUsername)
            |> andThen findByUsername
            |> map2 (flip profileObj) (HandlerData False)
