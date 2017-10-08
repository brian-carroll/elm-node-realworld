# Elm server-side experiment
Experiment to translate the RealWorld Node Express example app to Elm.
Just because I like Elm and I want to see if I can do this.

## Interactions with Express
- Router
    - Toplevel routing with `router.use('/thing', subRouter)`
    - Declare endpoints with `.get()`, `.post()`, `.put()`, `.delete()`
    - Declare URL params with `.param()`
        - Allows use of `:myParamName` in URLs
        - Attach `.myParamName` to `request.params` object
        - _Must be called before routes that use it_


- Elm routing
    - Union Types
    - Logic for controllers defined in Elm functions

- Elm/JS interface
    - Do I want Express to do the actual routing or not?
    - Could define my routes in Elm datastructures, then run an initialisation function on the tree
        - Express router will call the same handler for every route, then delegate to Elm, which dispatches depending on route
    - 

- Auth
    - Could just define it as a Union Type, optional or required

## What if I use just Node, not Express?
- Get the URL as a String and parse it into a Route structure
    - Probably get to use some of the 0.19 SPA routing stuff?
- Build up a Response structure
    - with status, head, and body

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
- Native module with Tasks
    - can use andThen and stuff
- Ports
    - Must be Commands
    - Two-step approach
        - Could have different Message types for before and after
    - Pseudo-tasks
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
