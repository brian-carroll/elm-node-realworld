module Framework.HandlerState exposing (..)

import Types exposing (..)
import Task exposing (Task)
import Json.Decode as JD
import Http


{-| Chain functions together in a pipeline to create an endpoint handler
Each stage of the pipeline can produce a different type of data
By the end of the pipeline, we need to have a specific type so that we can respond
to the client. But in the middle of a pipeline, the data can be any type, as long
as it's wrapped in a HandlerState.
-}
andThen : (a -> HandlerState x b) -> HandlerState x a -> HandlerState x b
andThen nextStage state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            nextStage data

        AwaitingPort jsEffect continuation ->
            AwaitingPort jsEffect (continuation >> andThen nextStage)

        AwaitingTask task ->
            AwaitingTask
                (task |> Task.map (andThen nextStage))


succeed : a -> HandlerState e a
succeed data =
    HandlerData data


fail : e -> HandlerState e a
fail err =
    HandlerError err


fromResult : (x -> y) -> Result x a -> HandlerState y a
fromResult errorMapper result =
    case result of
        Ok r ->
            HandlerData r

        Err e ->
            HandlerError (errorMapper e)


fromJson : (String -> e) -> JD.Decoder a -> JD.Value -> HandlerState e a
fromJson errorMapper decoder jsValue =
    JD.decodeValue decoder jsValue
        |> fromResult errorMapper


fromTask : (x -> y) -> Task x a -> HandlerState y a
fromTask errorMapper task =
    task
        |> Task.map HandlerData
        |> Task.onError
            (\error ->
                error
                    |> errorMapper
                    |> HandlerError
                    |> Task.succeed
            )
        |> AwaitingTask


fromHttp : (Http.Error -> e) -> Http.Request a -> HandlerState e a
fromHttp errorMapper request =
    request
        |> Http.toTask
        |> fromTask errorMapper


fromJsEffect : (String -> e) -> JD.Decoder a -> JsEffect -> HandlerState e a
fromJsEffect errorMapper decoder jsEffect =
    AwaitingPort jsEffect
        (\jsValue ->
            fromJson errorMapper decoder jsValue
        )


map : (a -> b) -> HandlerState e a -> HandlerState e b
map f state =
    state
        |> andThen (\data -> HandlerData (f data))


map2 : (a -> b -> c) -> HandlerState x a -> HandlerState x b -> HandlerState x c
map2 f a b =
    map f a
        |> andMap b


map3 :
    (a -> b -> c -> d)
    -> HandlerState x a
    -> HandlerState x b
    -> HandlerState x c
    -> HandlerState x d
map3 f a b c =
    map f a
        |> andMap b
        |> andMap c


map4 :
    (a -> b -> c -> d -> e)
    -> HandlerState x a
    -> HandlerState x b
    -> HandlerState x c
    -> HandlerState x d
    -> HandlerState x e
map4 f a b c d =
    map f a
        |> andMap b
        |> andMap c
        |> andMap d


andMap : HandlerState e a -> HandlerState e (a -> b) -> HandlerState e b
andMap hsx hsf =
    case hsf of
        HandlerData f ->
            map f hsx

        HandlerError e ->
            HandlerError e

        AwaitingPort jsEffect cont ->
            AwaitingPort jsEffect (cont >> (andMap hsx))

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (andMap hsx))


mapError : (x -> y) -> HandlerState x a -> HandlerState y a
mapError f state =
    case state of
        HandlerData a ->
            HandlerData a

        HandlerError e ->
            HandlerError (f e)

        AwaitingPort jsEffect cont ->
            AwaitingPort jsEffect (cont >> (mapError f))

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (mapError f))
