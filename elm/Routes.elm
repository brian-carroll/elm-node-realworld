module Routes exposing (..)


type Method
    = Get
    | Post
    | Put
    | Delete


type RouteAuth
    = AuthRequired
    | AuthOptional


type TopLevelPaths
    = Tags
    | Profiles
    | Articles
    | Users
    | User


type ProfilePaths
    = Follow


type UrlParam
    = Username String
    | Article String
    | Comment String


type PathSegment choices subpath
    = FixedSegment choices (List Endpoint) (List subpath)
    | ParameterSegment (String -> UrlParam) (List Endpoint) (List subpath)



-- parameter segments could have string wrapper type constructors
-- endpoints will need functions RequestBody -> UrlParams -> Response


type Endpoint
    = Endpoint Method RouteAuth


(</>) =
    FixedSegment


tree =
    (</>) ()
        []
        [ (</>) Tags
            [ Endpoint Get AuthOptional ]
            []
        , (</>) Profiles
            []
            [ ParameterSegment Username
                [ Endpoint Get AuthOptional ]
                [ (</>) Follow
                    [ Endpoint Post AuthRequired
                    , Endpoint Delete AuthRequired
                    ]
                    []
                ]
            ]
        , (</>) Articles [] []
        ]



{-
   Maybe what we need here is not really a big data structure but a big case expression.
   Parse each bit of the URL at a time, then pass to next module down, which parses the next bit
   Like reducers (update functions)

   I suppose they're just two different approaches.
    - Build a structure then traverse it.
        - Nice to have a single data structure with all the routes.
        - Code can still be split across files, but we do have a central _value_. Best of both worlds.
    - Nested reducers
        - Types are a lot simpler
        - More obvious how to structure the code
        - Seems easier to go bit by bit
        - Similar to normal Elm architecture. A URL is very like a message.


   Parser for Union Type is just a case expression from string to Union, with a default clause that dispatches a 404
-}
{-

   get  '/tags/'

   get  '/profiles/:username'  auth.optional
   post  '/profiles/:username/follow'  auth.required
   delete  '/profiles/:username/follow'  auth.required

   get  '/articles/'  auth.optional
   post  '/articles/'  auth.required

   get  '/articles/feed'  auth.required

   get  '/articles/:article'  auth.optional
   put  '/articles/:article'  auth.required
   delete  '/articles/:article'  auth.required

   post  '/articles/:article/favorite'  auth.required
   delete  '/articles/:article/favorite'  auth.required

   post  '/articles/:article/comments'  auth.required
   get  '/articles/:article/comments'  auth.optional
   delete  '/articles/:article/comments/:comment'  auth.required

   post  '/users'
   post  '/users/login'

   get  '/user'  auth.required
   put  '/user'  auth.required


-}
