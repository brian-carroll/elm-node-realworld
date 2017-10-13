module Models.User exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Regex exposing (regex)
import Types exposing (..)
import JsonWebToken as JWT
import Time


type alias User =
    { id : Maybe String
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
        (JD.maybe (JD.field "id" JD.string))
        (JD.field "username" decodeUsername)
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
    JE.object
        [ ( "type", JE.string "User" )
        , ( "username", encodeUsername user.username )
        , ( "email", encodeEmail user.email )
        , ( "bio", JE.string user.bio )
        , ( "image", JE.string user.image )
        , ( "hash", JE.string user.hash )
        , ( "salt", JE.string user.salt )
        ]


toAuthJSON : Secret -> Time.Time -> User -> Maybe JE.Value
toAuthJSON secret now user =
    case generateJWT secret now user of
        Nothing ->
            Nothing

        Just token ->
            Just <|
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
    { id : String
    , username : String
    , exp : Int
    }


payloadEncoder : JwtPayload -> JE.Value
payloadEncoder payload =
    JE.object
        [ ( "id", JE.string payload.id )
        , ( "username", JE.string payload.username )
        , ( "exp", JE.int payload.exp )
        ]


generateJWT : Secret -> Time.Time -> User -> Maybe String
generateJWT secret now user =
    case ( user.username, user.id ) of
        ( Username usernameStr, Just idStr ) ->
            Just <|
                JWT.encode
                    JWT.hmacSha256
                    payloadEncoder
                    secret
                    { id = idStr
                    , username = usernameStr
                    , exp = round ((now + (Time.hour * 24 * 14)) / 1000.0)
                    }

        ( _, Nothing ) ->
            Nothing


setPassword : User -> User
setPassword user =
    { user
        | hash = user.hash
        , salt = user.salt
    }


matchPassword : String -> String -> Bool
matchPassword p1 p2 =
    p1 == p2
