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
onError mapError state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData result ->
            case result of
                Ok data ->
                    HandlerData data

                Err e ->
                    mapError e

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> onError mapError)

        AwaitingTask task ->
            AwaitingTask
                (task |> Task.map (onError mapError))


{-| Try a computation that may fail (i.e. returns a Result), as part of a handler pipeline.
As the first argument, provide a function to transform the `Err` into a `HandlerError`

    import Json.Decode as JD

    HandlerData """{ "hello": "world" }"""
        |> try HandlerError (HandlerData << JD.decodeString (JD.field "hello" JD.string))

This is equivalent to

    import Json.Decode as JD

    HandlerData """{ "hello": "world" }"""
        |> andThen (HandlerData << JD.decodeString (JD.field "hello" JD.string))
        |> onError HandlerError

-}
try : (x -> HandlerState y b) -> (a -> Result x b) -> HandlerState y a -> HandlerState y b
try mapError f state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            case f data of
                Ok r ->
                    HandlerData r

                Err e ->
                    mapError e

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> try mapError f)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (try mapError f))


{-| Insert a Task into a pipeline

Example:

    userDecoder : Json.Decode.Decoder User

    fetchUser : String -> Task.Task
    fetchUser username =
        Http.get ("https://api.example.com/user/" ++ username) userDecoder
            |> Http.toTask

    pipeline : HandlerState Http.Error User
    pipeline =
        HandlerData "brian"
            |> tryTask HandlerError fetchUser

There is no equivalent of `onError` for `Task`s. It's better to handle success and error in one step.

-}
tryTask : (x -> HandlerState y b) -> (a -> Task x b) -> HandlerState y a -> HandlerState y b
tryTask mapError createTask state =
    case state of
        HandlerError e ->
            HandlerError e

        HandlerData data ->
            createTask data
                |> Task.map HandlerData
                |> Task.onError (\e -> Task.succeed (mapError e))
                |> AwaitingTask

        AwaitingPort outboundPortAction continuation ->
            AwaitingPort outboundPortAction (continuation >> tryTask mapError createTask)

        AwaitingTask task ->
            AwaitingTask (task |> Task.map (tryTask mapError createTask))


wrapErrString : ErrorCode -> String -> HandlerState EndpointError a
wrapErrString errCode message =
    HandlerError { status = errCode, messages = [ message ] }


fromJson : ErrorCode -> JD.Decoder a -> HandlerState EndpointError JD.Value -> HandlerState EndpointError a
fromJson errCode decoder jsonHandlerData =
    jsonHandlerData
        |> andThen (JD.decodeValue decoder >> HandlerData)
        |> onError (wrapErrString errCode)


{-| Create a `HandlerState` from two other `HandlerState`s, after unwrapping the data from each of them.

Example:

    -- Hash a password by calling out to a JS crypto library
    jsHashPassword : String -> HandlerState x PasswordHash
    jsHashPassword plainText =
        AwaitingPort (HashPassword plainText) (JD.decodeValue hashDecoder)

    -- Fetch the user data from a remote DB server using HTTP
    userDataFromDb : Int -> HandlerState x User
    userDataFromDb userId =
        AwaitingTask (fetchUserTask userId)

    -- Function that logs in a User, given a password hash
    -- The function operates on plain data, but we only have HandlerStates!
    loginUser : PasswordHash -> User -> HandlerState x LoginResponse
    loginUser hashedPassword dbUser =
        -- do stuff

    -- Use map2 to apply the loginUser function
    response =
        map2 loginUser loginFormData userDataFromDb

This is handy for breaking code into chunks and combine them back together again. It means we don't
always have to use a single sequential chain. Code ends up being nicer and more readable.

The arguments to the function are evaluated in order.

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
            AwaitingTask (task |> Task.map (\hs -> map2 f hs b))


{-| Same kind of thing as map2. Surprise!
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
            AwaitingTask (task |> Task.map (\hs -> map3 f hs b c))
