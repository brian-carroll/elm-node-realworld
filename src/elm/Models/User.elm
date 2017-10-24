module Models.User
    exposing
        ( User
        , Username
        , Email
        , HashAndSalt
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
import Time
import Regex exposing (regex)
import JsonWebToken as JWT


-- app imports

import Types exposing (..)
import HandlerState exposing (onError, wrapErrString)


type alias User =
    { id : Maybe Int
    , username : Username
    , email : Email
    , bio : String
    , image : Maybe String
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


decodeUser : Decoder User
decodeUser =
    JD.map7 User
        (JD.field "id" (JD.map Just JD.int))
        (JD.field "username" decodeUsername)
        (JD.field "email" decodeEmail)
        (JD.field "bio" JD.string)
        (JD.field "image" (JD.maybe JD.string))
        (JD.field "hash" JD.string)
        (JD.field "salt" JD.string)


encodeMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encodeMaybe encoder maybeVal =
    case maybeVal of
        Just x ->
            encoder x

        Nothing ->
            JE.null


encodeUserSqlValues : User -> List JE.Value
encodeUserSqlValues user =
    [ encodeMaybe JE.int user.id
    , encodeUsername user.username
    , encodeEmail user.email
    , JE.string user.bio
    , encodeMaybe JE.string user.image
    , JE.string user.hash
    , JE.string user.salt
    ]


decodeUserSqlResult : JD.Decoder (Result String User)
decodeUserSqlResult =
    JD.oneOf
        [ JD.map Ok (JD.field "result" decodeUser)
        , JD.map Err (JD.field "error" JD.string)
        ]


findByUsername : Username -> HandlerState EndpointError User
findByUsername u =
    case u of
        Username username ->
            AwaitingPort
                (SqlQuery
                    { sql = "SELECT * FROM users WHERE username=$1 LIMIT 1;"
                    , values = [ JE.string username ]
                    }
                )
                (HandlerData << JD.decodeValue decodeUserSqlResult)
                |> onError (\jsonError -> wrapErrString InternalError jsonError)
                |> onError (\dbError -> wrapErrString InternalError dbError)


findByEmail : Email -> HandlerState EndpointError User
findByEmail e =
    case e of
        Email email ->
            AwaitingPort
                (SqlQuery
                    { sql = "SELECT * FROM users WHERE email=$1 LIMIT 1;"
                    , values = [ JE.string email ]
                    }
                )
                (HandlerData << JD.decodeValue decodeUserSqlResult)
                |> onError (\jsonError -> wrapErrString InternalError jsonError)
                |> onError (\dbError -> wrapErrString InternalError dbError)


save : User -> HandlerState EndpointError User
save user =
    AwaitingPort
        (SqlQuery
            { sql =
                case user.id of
                    Nothing ->
                        "INSERT INTO users(username, email, bio, image, hash, salt) VALUES($2,$3,$4,$5,$6,$7) RETURNING *;"

                    Just id ->
                        "UPDATE users SET username=$2, email=$3, bio=$4, image=$5, hash=$6, salt=$7 WHERE id=$1 RETURNING *;"
            , values = encodeUserSqlValues user
            }
        )
        (HandlerData << JD.decodeValue decodeUserSqlResult)
        |> onError (\jsonError -> wrapErrString InternalError jsonError)
        |> onError (\dbError -> wrapErrString InternalError dbError)


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
            , ( "image", encodeMaybe JE.string user.image )
            , ( "token", JE.string token )
            ]


toProfileJSONFor : User -> User -> JE.Value
toProfileJSONFor viewingUser profileUser =
    JE.object
        [ ( "username", encodeUsername profileUser.username )
        , ( "bio", JE.string profileUser.bio )
        , ( "image", encodeMaybe JE.string profileUser.image )
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
