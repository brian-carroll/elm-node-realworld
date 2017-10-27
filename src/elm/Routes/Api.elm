module Routes.Api exposing (requireAuth)

-- library imports

import Dict


-- local imports

import Types exposing (..)
import HandlerState exposing (andThen, onError, wrapErrString)
import Models.User exposing (JwtPayload, verifyJWT)


requireAuth : Secret -> Connection -> HandlerState EndpointError JwtPayload
requireAuth secret conn =
    case Dict.get "authorization" conn.request.headers of
        Nothing ->
            wrapErrString Unauthorized "Authorization header required"

        Just authHeader ->
            case String.words authHeader of
                [ "Token", token ] ->
                    HandlerData token
                        |> andThen (verifyJWT secret conn.timestamp >> HandlerData)
                        |> onError (wrapErrString Unauthorized)

                _ ->
                    wrapErrString Unauthorized "Invalid token"
