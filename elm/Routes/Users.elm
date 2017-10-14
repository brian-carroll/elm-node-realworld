module Routes.Users exposing (..)

import UrlParser exposing (Parser, s, top, map, oneOf)
import Types exposing (..)
import Json.Encode as JE


type UsersRoute
    = Register
    | Login
    | CurrentUser



{-
   post  '/users'    -- create new user
   post  '/users/login'

   get  '/user'  auth.required   -- return most fields
   put  '/user'  auth.required   -- modify
-}


urlParserUsers : Parser (UsersRoute -> parserState) parserState
urlParserUsers =
    oneOf
        [ map Register top
        , map Login (s "login")
        ]


urlParserUser : Parser (UsersRoute -> parserState) parserState
urlParserUser =
    map CurrentUser top


dispatch : Method -> UsersRoute -> Connection -> HandlerState
dispatch method route conn =
    case route of
        Register ->
            Success JE.null

        _ ->
            Success JE.null
