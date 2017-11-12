module Models.Utils exposing (..)

import Regex
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Types exposing (..)
import Framework.HandlerState exposing (onError, wrapErrString)


matchesRegex : Regex.Regex -> (String -> a) -> String -> Decoder a
matchesRegex r constructor s =
    if Regex.contains r s then
        JD.succeed (constructor s)
    else
        JD.fail (toString (constructor s) ++ " is invalid")


runSqlQuery : JD.Decoder a -> { sql : String, values : List JE.Value } -> HandlerState EndpointError a
runSqlQuery decoder { sql, values } =
    AwaitingPort
        (SqlQuery
            { sql = sql
            , values = values
            }
        )
        (HandlerData << JD.decodeValue (decodeSqlResults decoder))
        |> onError (\jsonError -> wrapErrString InternalError jsonError)
        |> onError (\dbError -> wrapErrString InternalError dbError)


decodeSqlResults : JD.Decoder a -> JD.Decoder (Result String a)
decodeSqlResults rowsDecoder =
    JD.oneOf
        [ JD.map Ok
            (JD.field "result" <|
                JD.field "rows" <|
                    rowsDecoder
            )
        , JD.map Err (JD.field "error" JD.string)
        ]


decodeEmptyArray : JD.Decoder ()
decodeEmptyArray =
    JD.list (JD.succeed ())
        |> JD.andThen
            (\list ->
                if List.isEmpty list then
                    JD.succeed ()
                else
                    JD.fail "Array is not empty"
            )


encodeMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encodeMaybe encoder maybeVal =
    case maybeVal of
        Just x ->
            encoder x

        Nothing ->
            JE.null
