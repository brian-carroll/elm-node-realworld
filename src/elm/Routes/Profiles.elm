module Routes.Profiles exposing (..)

import UrlParser exposing (Parser, (</>), s, string, map, oneOf, parseString, top)
import Types exposing (..)


type ProfilesRoute
    = ShowProfile String
    | FollowProfile String



{-
   get  '/profiles/:username'  auth.optional
   post  '/profiles/:username/follow'  auth.required
   delete  '/profiles/:username/follow'  auth.required
-}


urlParser : Parser (ProfilesRoute -> parserState) parserState
urlParser =
    oneOf
        [ map ShowProfile (string)
        , map FollowProfile (string </> s "follow")
        ]
