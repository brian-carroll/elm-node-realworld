# Elm server-side experiment
Experiment to translate the RealWorld Node Express example app to Elm.
Just because I like Elm and I want to see if I can do this.

## Just Node, not Express
- Express feels like it's designed for mutation and dynamic types. Better to go bare-bones Node.
- Get the URL as a String and parse it into a Route structure
- Build up a Response structure
    - with status, head, and body
    - pass around a reference to the Node response object as an opaque Json.Encode.Value,
        then just pass it back out to JS when ready and call methods on it!

## Database
- Mongo
    - Elm JSON encode/decode for validation
    - Easier option

- SQL
    - Functions concatenating SQL strings?
    - Generate SQL using some sort of DSL (like html lib or elm-css)
        - Can probably find similar in Haskell / F# / OCaml / PureScript
    - SQL files, imported on startup? What about text substitution?

- CouchDB
    - Has a HTTP & JSON interface! Nice!
    - Can just use it with normal Elm, even Tasks :)


## Async ports
- How to do effects in the middle of a chain of operations while maintaining its association with
   a particular request? `Task`s can do that via `andThen`, but `Cmd`s can't.
- Use native/kernel modules so I can create `Task`s?
    - Decided this was a nightmare.
    - Murphy Randle seemed to back that up shortly afterwards at Elm Conf US 2017!
- Ports
    - Ports must be `Cmd`s, can't be `Task`s
    - Callbacks
        - When you create a command, you also have to give it a `Msg` constructor function (`a -> Msg`)
        - That constructor function doesn't have to be a simple type constructor as usual. It can be arbitrarily complex. In fact it can be a callback representing _everything_ you want to do after you get the effect data, until you're ready to generate a response. (I think this is what they call 'continuation passing style')
    - Rejected idea: Pseudo-tasks
        - Port command has to carry data only, but we can put a callback function in the app State
        - Give each command a unique ID, pass it out to JS and back.
        - Store the callback closures in a Dict on the state against the unique ID
        - When the return port Message comes in, pluck the closure out of the Dict and apply it to the data
            - Need to JSON decode it first
        - Function stored in the state can be as general as `Json.value -> (Model, Cmd)`

## Connections
- There is normally only one Elm instance
    - could re-init each time but that's slow
- Responses could come out of Elm in a different order than requests go in, since we're doing async stuff
- We need to pass some connection identifier into Elm along with the request
    - Keep a dictionary object with UUID keys on the JS side, then pass the ID through Elm?
    - Pass the actual object reference?
        - Can I pass a Json.value into Elm and back out?
        - Yes. Yes I can.


## Pipeline
- Decode request (method, url, headers, body)
- Parse route
- From route and method, choose pipeline
- Run each transformation in pipeline
- Pipeline transformation is either middleware or endpoint
    - Middleware = Connection -> Connection
    - Endpoint = Connection -> Response
- Any Middleware or Endpoint can generate a command and continuation function
- Would be nice to have a dummy continuation with no effects
- perform Task.succeed with a message
- Connection could hold extra meta data
- might be able to use extensible records to get some polymorphism
- If a pipeline stage needs certain inputs it can put constraints on the metadata record type

## How to handle route?
- Would be nice to add route to Connection.request

## Example pipeline for creating user
- Decode request (method, url, headers, body)
- Parse route
- From route and method, choose pipeline
- Pipeline
    - validate input JSON (pure)
    - generate random salt (Elm effect)
    - hash password (pure)
    - assemble DB JSON (pure)
    - send JSON to DB (HTTP effect, task)
    - receive DB response (task andThen)
    - transform DB JSON to response JSON (pure)
    - respond to user (port effect)

So if a pipeline stage returns an effect then the next stage
needs to be a function of a matching type
Very monadic

Can't do this as a list, needs to be a composition
stage1 >> stage2 >> stage3

doCmdAndContinue : Conn -> a -> Cmd Msg


stage1 : Conn -> Cmd (Conn -> a -> Msg)
stage2 : Conn -> a -> (Conn, Cmd )


## experiments
- do a trivial command (task.succeed)


dummyCmd : (a -> Msg) -> a -> Cmd Msg
dummyCmd msg payload =
  Task.perform msg (Task.succeed payload)

## Next steps
- pipeline stage output type, ideas
```elm
    (
        -- base type fields
        { request : Request
        , response : Response
        , timestamp : Time
        , randomSeed : Random.Seed
        , route : Route

        -- optional stuff added by middleware
        , user : User
        , auth : LoggedIn | Guest
        
        -- idea
        , effects : ToRun (Cmd msg) | Return Json.Decode.Value
        }
    , Cmd Msg
    )
```

## Nice JS interop
- Only actually need it for registration
    - Out of all the 12 endpoints, just one needs to go out to JS for effects
    - Basically it's all about the DB

### Pass all data to JS, remember nothing in Elm
- We can't give it a callback, and we can't compare JS.Value's
- Elm will have to re-decode the connection at the gate, and route it to a 'step 2' of the endpoint

### Remember stuff in Elm state, waiting for JS to get back.
- We can remember functions, not just values
- Allows a Monadic composable version
- Need a Connection ID as Dict key
    - Could use an `(Int, Int)` key, built from timestamp and a counter in Node
    - These could be separate fields in the Connection, just assemble into a Tuple for storage
- Avoid leaks if JS blows up
    - Expiration time?
    - Garbage collection cycle triggered from time subscription?
    - setTimeout, then delete from Dict & **auto-respond with a HTTP timeout status!!**
    - implementation details
        - Elm can only do setInterval, but it can also check the model and unsubscribe if it wants
        - Dict => all values have to have exact same concrete type
        - BUT could work around that using closures
        - The type we want to store is `Json.Decode.Value -> JsOperation`
        - We always want another JS operation. It can be either effect or response
        - Produce these closures using `runJsAndThen`, which builds them from `Plugs`

### What would a nice API look like?
- best would be some kind of `doEffectAndThen` or `runJsAndThen`
    - how would I like to write this?
        ```elm
        collectUnderpants : Connection stuff0 -> (Connection stuff1, Cmd)
        profit : Connection stuff1 -> effectData -> Connection stuff2

        endpoint =
            collectUnderpants
            |> runCommandAndThen profit
        ```
    - or possibly
        ```elm
        type JsOperation
            = Respond Response
            | HashPassword String

        collectUnderpants : Connection stuff0 -> (Connection stuff1, JsOperation)
        profit : Connection stuff1 -> Json.Decode.Value -> Connection stuff2

        endpoint =
            collectUnderpants
            |> runJsAndThen profit
        ```
