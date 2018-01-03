module Routes.Api exposing (requireAuth, encodeDate, error)

-- library imports

import Dict
import Date exposing (Date, Month(..))
import Json.Encode as JE


-- local imports

import Types exposing (..)
import Framework.HandlerState exposing (map, andThen, mapError, fromResult, fail)
import Models.User exposing (JwtPayload, verifyJWT)


error : ErrorCode -> String -> EndpointError
error errCode message =
    { status = errCode, messages = [ message ] }


requireAuth : Secret -> Connection -> HandlerState EndpointError JwtPayload
requireAuth secret conn =
    case Dict.get "authorization" conn.request.headers of
        Nothing ->
            fail (error Unauthorized "Authorization header required")

        Just authHeader ->
            case String.words authHeader of
                [ "Token", token ] ->
                    verifyJWT secret conn.timestamp token
                        |> fromResult (error Unauthorized)

                _ ->
                    fail (error Unauthorized "Invalid token")


digits : Int -> Int -> String
digits places num =
    String.padLeft places '0' (toString num)


encodeDate : Date -> JE.Value
encodeDate date =
    let
        month =
            case Date.month date of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12
    in
        -- 2016-02-18T03:22:56.637Z
        JE.string
            (toString (Date.year date)
                ++ "-"
                ++ digits 2 month
                ++ "-"
                ++ digits 2 (Date.day date)
                ++ "T"
                ++ digits 2 (Date.hour date)
                ++ ":"
                ++ digits 2 (Date.minute date)
                ++ ":"
                ++ digits 2 (Date.second date)
                ++ "."
                ++ digits 3 (Date.millisecond date)
                ++ "Z"
            )
