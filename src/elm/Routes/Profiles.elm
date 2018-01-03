module Routes.Profiles exposing (ProfilesRoute, routeParser, dispatch)

-- local imports

import Types exposing (..)
import Framework.HandlerState as HS
import Framework.RouteParser exposing (Parser, Method(..), m, (</>), s, string, map, oneOf)
import Models.User exposing (User, Username(..))


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
                |> HS.andThen Models.User.findByUsername

        isFollowing =
            case authUsername of
                HandlerError _ ->
                    HandlerData False

                _ ->
                    authUsername
                        |> HS.andThen (flip Models.User.isFollowing profileUsername)
    in
        HS.map2 Models.User.profileObj
            profileUser
            isFollowing


followProfile : HandlerState EndpointError Username -> Username -> Connection -> EndpointState
followProfile authUsername profileUsername conn =
    let
        writeFollowToDb =
            authUsername
                |> HS.andThen (flip Models.User.follow profileUsername)
    in
        writeFollowToDb
            |> HS.map (\_ -> profileUsername)
            |> HS.andThen Models.User.findByUsername
            |> HS.map (flip Models.User.profileObj True)


unfollowProfile : HandlerState EndpointError Username -> Username -> Connection -> EndpointState
unfollowProfile authUsername profileUsername conn =
    let
        deleteFollowFromDb =
            authUsername
                |> HS.map (flip Models.User.unfollow profileUsername)
    in
        deleteFollowFromDb
            |> HS.map (\_ -> profileUsername)
            |> HS.andThen Models.User.findByUsername
            |> HS.map (flip Models.User.profileObj False)
