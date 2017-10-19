module HandlerState exposing (..)

import Types exposing (..)
import Task exposing (Task)
import Json.Decode as JD


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

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> andThen nextStage)

        AwaitingTask task ->
            AwaitingTask
                (task |> Task.map (andThen nextStage))


{-| Deal with a Result from a previous stage in the pipeline
If the Result is an `Ok`, turn it into a `HandlerData`
If the Result is an `Err`, turn it into a `HandlerError`, using the supplied function
-}
onError : (y -> HandlerState x a) -> HandlerState x (Result y a) -> HandlerState x a
onError wrapError state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData result ->
            case result of
                Ok data ->
                    HandlerData data

                Err e ->
                    wrapError e

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> onError wrapError)

        AwaitingTask task ->
            AwaitingTask
                (task |> Task.map (onError wrapError))


tryTask : (x -> HandlerState y b) -> (a -> Task x b) -> HandlerState y a -> HandlerState y b
tryTask liftError createTask state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            createTask data
                |> Task.map HandlerData
                |> Task.onError (\e -> Task.succeed (liftError e))
                |> AwaitingTask

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> tryTask liftError createTask)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (tryTask liftError createTask))


wrapErrString : ErrorCode -> String -> HandlerState EndpointError a
wrapErrString errCode message =
    HandlerError { status = errCode, messages = [ message ] }


fromJson : ErrorCode -> JD.Decoder a -> HandlerState EndpointError JD.Value -> HandlerState EndpointError a
fromJson errCode decoder jsonHandlerData =
    jsonHandlerData
        |> andThen (JD.decodeValue decoder >> HandlerData)
        |> onError (wrapErrString errCode)


{-| map2 : (a -> b -> HandlerState x c) -> HandlerState x a -> HandlerState x b -> HandlerState x c

Create a HandlerState from two other HandlerStates, after unwrapping the data from each of them.

This is handy for breaking code into chunks and combine them back together again. It means we don't
always have to use a single sequential chain. Code ends up being nicer and more readable.

The arguments to the function are evaluated in order.

Example:

    loginFormData : HandlerState x a
    loginFormData =
        -- JSON validation that might fail

    userDataFromDb : HandlerState x b
    userDataFromDb =
        -- Result of some port or task

    loginUser : a -> b -> HandlerState x c
    loginUser formData dbData =
        -- do something with both pieces of data and create a response

    -- combine it all together
    response =
        map2 loginUser loginFormData userDataFromDb

-}
map2 : (a -> b -> HandlerState x c) -> HandlerState x a -> HandlerState x b -> HandlerState x c
map2 f a b =
    case a of
        HandlerData da ->
            b |> andThen (f da)

        HandlerError x ->
            HandlerError x

        AwaitingPort action cont ->
            AwaitingPort action (cont >> (\hs -> map2 f hs b))

        AwaitingTask task ->
            AwaitingTask (task |> Task.andThen (\hs -> Task.succeed (map2 f hs b)))


{-| map3 : (a -> b -> c -> HandlerState x d) -> HandlerState x a -> HandlerState x b -> HandlerState x c -> HandlerState x d
Same kind of thing as map2. Surprise!
-}
map3 : (a -> b -> c -> HandlerState x d) -> HandlerState x a -> HandlerState x b -> HandlerState x c -> HandlerState x d
map3 f a b c =
    case a of
        HandlerData da ->
            map2 (f da) b c

        HandlerError x ->
            HandlerError x

        AwaitingPort action cont ->
            AwaitingPort action (cont >> (\hs -> map3 f hs b c))

        AwaitingTask task ->
            AwaitingTask (task |> Task.andThen (\hs -> Task.succeed (map3 f hs b c)))
