module Models.User
    exposing
        ( User
        , Username
        , Email
        , findByEmail
        , findByUsername
        , decodeEmail
        , decodeUsername
        , decodeHashAndSalt
        , toAuthJSON
        , verifyJWT
        , JwtPayload
        , save
        )

{-
      redefine in SQL:
          - findByEmail        select * from user where email=$1  , then decode DB JSON & handle DB errors
          - findByUsername     select * from user where email=$1  , then decode DB JSON & handle DB errors
          - save               upsert

   insert into pinned_tweets (user_handle, tweet_id, pinned_at)
     values (
       'rey',
       5,
       clock_timestamp()
     )
   on conflict (user_handle)
   do update set (tweet_id, pinned_at) = (5, clock_timestamp())
   where pinned_tweets.user_handle = 'rey';


-}
-- library imports

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Task exposing (Task)
import Time
import Http
import Regex exposing (regex)
import JsonWebToken as JWT


-- app imports

import Types exposing (..)
import Database


type alias User =
    { rev : Maybe String
    , username : Username
    , email : Email
    , bio : String
    , image : String
    , hash : String
    , salt : String
    }


type Username
    = Username String


type Email
    = Email String


matchesRegex : Regex.Regex -> (String -> a) -> String -> Decoder a
matchesRegex r constructor s =
    if Regex.contains r s then
        JD.succeed (constructor s)
    else
        JD.fail (toString (constructor s) ++ " is invalid")


decodeUsername : Decoder Username
decodeUsername =
    JD.string
        |> JD.andThen
            (String.toLower
                >> matchesRegex
                    (regex "^[a-z0-9]+$")
                    Username
            )


decodeEmail : Decoder Email
decodeEmail =
    JD.string
        |> JD.andThen
            (String.toLower
                >> matchesRegex
                    (regex "^\\S+@\\S+\\.\\S+$")
                    Email
            )


decodeUser : Decoder User
decodeUser =
    JD.map7 User
        (JD.maybe (JD.field "_rev" JD.string))
        (JD.field "_id" decodeUsername)
        (JD.field "email" decodeEmail)
        (JD.field "bio" JD.string)
        (JD.field "image" JD.string)
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


encodeUsername : Username -> JE.Value
encodeUsername username =
    case username of
        Username str ->
            JE.string str


encodeEmail : Email -> JE.Value
encodeEmail email =
    case email of
        Email str ->
            JE.string str


toDatabaseJSON : User -> JE.Value
toDatabaseJSON user =
    let
        emailId =
            case user.email of
                Email str ->
                    JE.string <| "email:" ++ str

        userId =
            case user.username of
                Username str ->
                    JE.string <| "user:" ++ str
    in
        Debug.log "databaseJSON" <|
            JE.object
                [ ( "docs"
                  , JE.list
                        [ JE.object
                            [ ( "_id", emailId )
                            , ( "type", JE.string "Email" )
                            , ( "user", userId )
                            ]
                        , JE.object <|
                            (case user.rev of
                                Nothing ->
                                    []

                                Just rev ->
                                    [ ( "_rev", JE.string rev ) ]
                            )
                                ++ [ ( "_id", userId )
                                   , ( "type", JE.string "User" )
                                   , ( "email", encodeEmail user.email )
                                   , ( "bio", JE.string user.bio )
                                   , ( "image", JE.string user.image )
                                   , ( "hash", JE.string user.hash )
                                   , ( "salt", JE.string user.salt )
                                   ]
                        ]
                  )
                ]


dbUserDocDecoder : Decoder User
dbUserDocDecoder =
    JD.map7 User
        (JD.field "_rev" (JD.map Just JD.string))
        (JD.field "_id" decodeUsernameFromId)
        (JD.field "email" decodeEmail)
        (JD.field "bio" JD.string)
        (JD.field "image" JD.string)
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


dBUserByEmailDecoder : Decoder User
dBUserByEmailDecoder =
    JD.field "rows" <|
        JD.index 0 <|
            JD.field "doc" dbUserDocDecoder


dropPrefix : String -> String -> String
dropPrefix prefix str =
    String.dropLeft
        (String.length prefix)
        str


decodeUsernameFromId : Decoder Username
decodeUsernameFromId =
    JD.string
        |> JD.andThen
            (\usernameId ->
                usernameId
                    |> dropPrefix "user:"
                    |> Username
                    |> JD.succeed
            )


decodeEmailFromId : Decoder Email
decodeEmailFromId =
    JD.string
        |> JD.andThen
            (\emailId ->
                emailId
                    |> dropPrefix "email:"
                    |> Email
                    |> JD.succeed
            )


findByUsername : Username -> Task Http.Error User
findByUsername username =
    let
        usernameId =
            case username of
                Username str ->
                    "user:" ++ str

        url =
            Debug.log "findByUsername" <|
                Database.dbUrl
                    ++ "/"
                    ++ usernameId
    in
        Http.get url dbUserDocDecoder
            |> Http.toTask


findByEmail : Email -> Task Http.Error User
findByEmail email =
    let
        emailStr =
            case email of
                Email str ->
                    str

        url =
            Debug.log "findByEmail, URL"
                (Database.dbUrl
                    ++ "/_design/usersByEmail/_view/users-by-email?include_docs=true&key=\"email:"
                    ++ emailStr
                    ++ "\""
                )
    in
        Http.get url dBUserByEmailDecoder
            |> Http.toTask


save : User -> Task Http.Error Database.DbPostBulkResponse
save user =
    user
        |> toDatabaseJSON
        |> Database.postBulkDocs
        |> Http.toTask


toAuthJSON : Secret -> Time.Time -> User -> JE.Value
toAuthJSON secret now user =
    let
        token =
            generateJWT secret now user
    in
        JE.object
            [ ( "username", (encodeUsername user.username) )
            , ( "email", encodeEmail user.email )
            , ( "bio", JE.string user.bio )
            , ( "image", JE.string user.image )
            , ( "token", JE.string token )
            ]


toProfileJSONFor : User -> User -> JE.Value
toProfileJSONFor viewingUser profileUser =
    JE.object
        [ ( "username", encodeUsername profileUser.username )
        , ( "bio", JE.string profileUser.bio )
        , ( "image", JE.string profileUser.image )
        , ( "following", JE.bool False )
        ]


type alias HashAndSalt =
    { hash : String
    , salt : String
    }


decodeHashAndSalt : JD.Decoder HashAndSalt
decodeHashAndSalt =
    JD.map2 HashAndSalt
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


type alias JwtPayload =
    { username : Username
    , exp : Int
    }


jwtPayloadEncoder : JwtPayload -> JE.Value
jwtPayloadEncoder payload =
    JE.object
        [ ( "username", encodeUsername payload.username )
        , ( "exp", JE.int payload.exp )
        ]


jwtPayloadDecoder : Decoder JwtPayload
jwtPayloadDecoder =
    JD.map2 JwtPayload
        (JD.field "username" decodeUsername)
        (JD.field "exp" JD.int)


generateJWT : Secret -> Time.Time -> User -> String
generateJWT secret now user =
    JWT.encode
        JWT.hmacSha256
        jwtPayloadEncoder
        secret
        { username = user.username
        , exp = round ((now + Time.hour * 24 * 14) / 1000)
        }


verifyJWT : Secret -> Time.Time -> String -> Result String JwtPayload
verifyJWT secret now token =
    case JWT.decode jwtPayloadDecoder secret token of
        Ok payload ->
            if payload.exp < round (now / 1000) then
                Err "Token expired"
            else
                Ok payload

        Err e ->
            Err <| Debug.log (toString e) "Token invalid"
