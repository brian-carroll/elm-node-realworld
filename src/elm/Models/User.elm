module Models.User exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Regex exposing (regex)
import Types exposing (..)
import JsonWebToken as JWT
import Time
import Database
import Http
import Task exposing (Task)


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
        JE.object
            [ ( "docs"
              , JE.list
                    [ JE.object
                        [ ( "_id", emailId )
                        , ( "type", JE.string "Email" )
                        , ( "user", userId )
                        ]
                    , JE.object
                        [ ( "_id", userId )
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
            Debug.log "user url" <|
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
