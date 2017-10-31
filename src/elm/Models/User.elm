module Models.User
    exposing
        ( User
        , UserId(..)
        , Username(..)
        , Email(..)
        , HashAndSalt
        , JwtPayload
        , findByEmail
        , findByUsername
        , decodeEmail
        , decodeUsername
        , decodeHashAndSalt
        , authObj
        , profileObj
        , verifyJWT
        , save
        , isFollowing
        , follow
        , unfollow
        )

-- library imports

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Time
import Regex exposing (regex)
import JsonWebToken as JWT


-- app imports

import Types exposing (..)
import HandlerState exposing (onError, wrapErrString)
import Models.Utils exposing (matchesRegex)


type alias User =
    { id : UserId
    , username : Username
    , email : Email
    , bio : String
    , image : Maybe String
    , hash : String
    , salt : String
    }


type UserId
    = UserId Int
    | UnsavedUserId


type Username
    = Username String


type Email
    = Email String


decodeUserId : Decoder UserId
decodeUserId =
    JD.map UserId JD.int


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
        (JD.field "id" decodeUserId)
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
    (case user.id of
        UserId id ->
            [ JE.int id ]

        UnsavedUserId ->
            []
    )
        ++ [ encodeUsername user.username
           , encodeEmail user.email
           , JE.string user.bio
           , encodeMaybe JE.string user.image
           , JE.string user.hash
           , JE.string user.salt
           ]


decodeSqlResults : JD.Decoder a -> JD.Decoder (Result String a)
decodeSqlResults rowsDecoder =
    JD.oneOf
        [ JD.map Ok
            (JD.field "result" <|
                JD.field "rows" <|
                    rowsDecoder
            )
        , JD.map Err (JD.field "error" JD.string)
        ]


decodeEmptyArray : JD.Decoder ()
decodeEmptyArray =
    JD.list (JD.succeed ())
        |> JD.andThen
            (\list ->
                if List.isEmpty list then
                    JD.succeed ()
                else
                    JD.fail "Array is not empty"
            )


runSqlQuery : JD.Decoder a -> { sql : String, values : List JE.Value } -> HandlerState EndpointError a
runSqlQuery decoder { sql, values } =
    AwaitingPort
        (SqlQuery
            { sql = sql
            , values = values
            }
        )
        (HandlerData << JD.decodeValue (decodeSqlResults decoder))
        |> onError (\jsonError -> wrapErrString InternalError jsonError)
        |> onError (\dbError -> wrapErrString InternalError dbError)


findByUsername : Username -> HandlerState EndpointError User
findByUsername u =
    case u of
        Username username ->
            runSqlQuery (JD.index 0 decodeUser)
                { sql = "SELECT * FROM users WHERE username=$1 LIMIT 1;"
                , values = [ JE.string username ]
                }


findByEmail : Email -> HandlerState EndpointError User
findByEmail e =
    case e of
        Email email ->
            runSqlQuery (JD.index 0 decodeUser)
                { sql = "SELECT * FROM users WHERE email=$1 LIMIT 1;"
                , values = [ JE.string email ]
                }


save : User -> HandlerState EndpointError User
save user =
    runSqlQuery (JD.index 0 decodeUser)
        { sql =
            case user.id of
                UnsavedUserId ->
                    """
                    INSERT INTO users(username, email, bio, image, hash, salt)
                    VALUES($1,$2,$3,$4,$5,$6) RETURNING *;
                    """

                UserId _ ->
                    """
                    UPDATE users SET
                    username=$2, email=$3, bio=$4, image=$5, hash=$6, salt=$7
                    WHERE id=$1 RETURNING *;
                    """
        , values = encodeUserSqlValues user
        }


follow : Username -> Username -> HandlerState EndpointError ()
follow currentUsername profileUsername =
    runSqlQuery decodeEmptyArray
        { sql = """
            insert into follows(follower_id,followed_id) values
                ( (select id as follower_id from users where username=$1)
                , (select id as followed_id from users where username=$2)
                );
            """
        , values =
            [ encodeUsername currentUsername
            , encodeUsername profileUsername
            ]
        }


unfollow : Username -> Username -> HandlerState EndpointError ()
unfollow currentUsername profileUsername =
    runSqlQuery decodeEmptyArray
        { sql = """
            delete from follows where
                follower_id=(select id from users where username=$1)
                AND
                followed_id=(select id from users where username=$2);
            """
        , values =
            [ encodeUsername currentUsername
            , encodeUsername profileUsername
            ]
        }


isFollowing : Username -> Username -> HandlerState EndpointError Bool
isFollowing currentUsername profileUsername =
    runSqlQuery (JD.index 0 (JD.field "following" JD.bool))
        { sql = """
            SELECT COUNT(*)>0 as following FROM
                users AS followers
                INNER JOIN follows
                    ON followers.id=follows.follower_id
                INNER JOIN users AS followed
                    ON followed.id=follows.followed_id
            WHERE
                followers.username=$1 AND followed.username=$2;
            """
        , values =
            [ encodeUsername currentUsername
            , encodeUsername profileUsername
            ]
        }


authObj : Secret -> Time.Time -> User -> JE.Value
authObj secret now user =
    let
        token =
            generateJWT secret now user
    in
        JE.object <|
            [ ( "user"
              , JE.object
                    [ ( "username", (encodeUsername user.username) )
                    , ( "email", encodeEmail user.email )
                    , ( "bio", JE.string user.bio )
                    , ( "image", encodeMaybe JE.string user.image )
                    , ( "token", JE.string token )
                    ]
              )
            ]


profileObj : User -> Bool -> HandlerState x JE.Value
profileObj profileUser isFollowing =
    let
        image =
            Maybe.withDefault
                "https://static.productionready.io/images/smiley-cyrus.jpg"
                profileUser.image
    in
        HandlerData <|
            JE.object <|
                [ ( "profile"
                  , JE.object
                        [ ( "username", encodeUsername profileUser.username )
                        , ( "bio", JE.string profileUser.bio )
                        , ( "image", JE.string image )
                        , ( "following", JE.bool isFollowing )
                        ]
                  )
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
