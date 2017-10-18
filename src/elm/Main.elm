port module Main exposing (main)

import Json.Decode as JD
import Json.Encode as JE
import Connection exposing (decodeConnection, errorResponse, successResponse, encodeResponse)
import Types exposing (..)
import Task exposing (Task)
import Dict
import Time exposing (Time)
import Routes


port elmToJs : JsInterface -> Cmd msg


port jsToElm : (JsInterface -> msg) -> Sub msg


main : Program ProgramConfig State Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : ProgramConfig -> ( State, Cmd Msg )
init config =
    ( { config = config
      , pending = Dict.empty
      }
    , Cmd.none
    )


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch <|
        (jsToElm ReceiveFromJs)
            :: (if Dict.isEmpty state.pending then
                    []
                else
                    [ Time.every Time.second CollectGarbage ]
               )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        ( pending, cmd ) =
            case msg of
                ReceiveFromJs jsData ->
                    updateJsData state jsData

                HandlerTaskResult conn handlerState ->
                    updateHandlerState conn state.pending handlerState

                CollectGarbage now ->
                    updateGarbageCollection state.pending now
    in
        ( { state | pending = pending }, cmd )


updateHandlerState : Connection -> PendingHandlers -> HandlerState EndpointError JD.Value -> ( PendingHandlers, Cmd Msg )
updateHandlerState conn pendingHandlers handlerState =
    case Debug.log "handlerState" handlerState of
        HandlerData json ->
            ( pendingHandlers
            , jsActionCmd RespondToClient <|
                successResponse json conn
            )

        HandlerError endpointError ->
            ( pendingHandlers
            , jsActionCmd RespondToClient <|
                errorResponse endpointError conn
            )

        AwaitingPort outboundPortAction continuation ->
            ( Dict.insert conn.id ( conn, continuation ) pendingHandlers
            , jsActionCmd outboundPortAction conn
            )

        AwaitingTask task ->
            ( pendingHandlers
            , Task.perform (HandlerTaskResult conn) task
            )


updateJsData : State -> JsInterface -> ( PendingHandlers, Cmd Msg )
updateJsData state jsData =
    case decodeDataFromJs jsData of
        NewConnection conn ->
            (Routes.dispatch state.config conn)
                |> updateHandlerState conn state.pending

        JsActionResult connId jsValue ->
            case Dict.get connId state.pending of
                Just ( connection, continueWith ) ->
                    continueWith jsValue
                        |> updateHandlerState
                            connection
                            (Dict.remove connId state.pending)

                Nothing ->
                    Debug.log ("Pending connection not found:\n" ++ toString connId)
                        ( state.pending, Cmd.none )

        InboundPortError str ->
            let
                _ =
                    Debug.log "InboundPortError" str
            in
                ( state.pending, Cmd.none )


decodeDataFromJs : JsInterface -> InboundPortData
decodeDataFromJs jsData =
    let
        decoder =
            case jsData.tag of
                "NewConnection" ->
                    JD.map NewConnection (decodeConnection jsData.connectionId)

                "JsActionResult" ->
                    JD.map2 JsActionResult
                        (JD.succeed jsData.connectionId)
                        JD.value

                "JsError" ->
                    JD.map InboundPortError JD.string

                _ ->
                    JD.fail "Unknown tag, JS -> Elm"
    in
        case JD.decodeValue decoder jsData.payload of
            Ok portData ->
                portData

            Err str ->
                InboundPortError str


jsActionCmd : OutboundPortAction -> Connection -> Cmd Msg
jsActionCmd elmData conn =
    elmToJs
        { connectionId = conn.id
        , tag = toString elmData |> String.words |> List.head |> Maybe.withDefault ""
        , payload =
            case elmData of
                RespondToClient ->
                    encodeResponse conn.response

                HashPassword plain ->
                    JE.string plain

                CheckPassword { hash, salt, plainText } ->
                    JE.object
                        [ ( "hash", JE.string hash )
                        , ( "salt", JE.string salt )
                        , ( "plainText", JE.string plainText )
                        ]
        }


updateGarbageCollection : PendingHandlers -> Time -> ( PendingHandlers, Cmd Msg )
updateGarbageCollection pending now =
    let
        predicate ( timestamp, seq ) val =
            now - timestamp > Time.second

        ( keep, dump ) =
            Dict.partition predicate pending

        dumpCmds =
            Dict.foldl
                (\key value acc -> dumpPendingHandler value :: acc)
                []
                (Debug.log "Collecting garbage" dump)
    in
        ( keep, Cmd.batch dumpCmds )


dumpPendingHandler : ( Connection, a ) -> Cmd Msg
dumpPendingHandler ( conn, _ ) =
    jsActionCmd RespondToClient <|
        errorResponse
            { status = InternalError
            , messages = [ "JS timeout" ]
            }
            conn
