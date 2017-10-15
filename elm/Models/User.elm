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
                        , ( "bio", JE.string user.bio )
                        , ( "image", JE.string user.image )
                        , ( "hash", JE.string user.hash )
                        , ( "salt", JE.string user.salt )
                        ]
                    ]
              )
            ]


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


type alias JwtPayload =
    { username : String
    , exp : Int
    }


payloadEncoder : JwtPayload -> JE.Value
payloadEncoder payload =
    JE.object
        [ ( "username", JE.string payload.username )
        , ( "exp", JE.int payload.exp )
        ]


generateJWT : Secret -> Time.Time -> User -> String
generateJWT secret now user =
    case user.username of
        Username usernameStr ->
            JWT.encode
                JWT.hmacSha256
                payloadEncoder
                secret
                { username = usernameStr
                , exp = round ((now + Time.hour * 24 * 14) / 1000)
                }


setPassword : User -> User
setPassword user =
    { user
        | hash = user.hash
        , salt = user.salt
    }


matchPassword : String -> String -> Bool
matchPassword p1 p2 =
    p1 == p2
