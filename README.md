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
