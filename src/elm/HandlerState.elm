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

        --b
        AwaitingTask task ->
            AwaitingTask
                (task |> Task.map (andThen nextStage))



--a


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


fromJson : JD.Decoder a -> ErrorCode -> HandlerState EndpointError JD.Value -> HandlerState EndpointError a
fromJson decoder errCode jsonHandlerData =
    jsonHandlerData
        |> andThen (JD.decodeValue decoder >> HandlerData)
        |> onError (wrapErrString errCode)
