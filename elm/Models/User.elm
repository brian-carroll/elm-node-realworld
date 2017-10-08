module Models.User exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Regex exposing (regex)
import Word.Bytes as Bytes
import Word.Hex as Hex
import Crypto.HMAC as HMAC
import Bitwise
import Random


config =
    { passwordHashIterations = 100
    , passwordHashBytes = 512
    }


type alias User =
    { username : Username
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
    JD.map6 User
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


toAuthJSON : User -> JE.Value
toAuthJSON user =
    JE.object
        [ ( "username", encodeUsername user.username )
        , ( "email", encodeEmail user.email )
        , ( "bio", JE.string user.bio )
        , ( "image", JE.string user.image )
        , ( "token", JE.string <| generateJWT user )
        ]


toProfileJSONFor : User -> User -> JE.Value
toProfileJSONFor viewingUser profileUser =
    JE.object
        [ ( "username", encodeUsername profileUser.username )
        , ( "bio", JE.string profileUser.bio )
        , ( "image", JE.string profileUser.image )
        , ( "following", JE.bool False )
        ]


generateJWT : User -> String
generateJWT user =
    -- TODO: make this actually generate a JWT
    -- Needs to go and get current date
    user.hash


getRandomSaltBytes : Int -> (List Int -> msg) -> Cmd msg
getRandomSaltBytes length msgConstructor =
    Random.generate msgConstructor <|
        Random.list length (Random.int 0 255)


hashPassword : String -> List Int -> { hash : String, salt : String }
hashPassword plainText saltBytes =
    { hash =
        pbkdf2
            plainText
            saltBytes
            config.passwordHashIterations
            config.passwordHashBytes
            HMAC.sha512
    , salt = Hex.fromByteList saltBytes
    }


pbkdf2 : String -> List Int -> Int -> Int -> HMAC.Hash -> String
pbkdf2 password saltBytes iterations keylen algo =
    let
        passwordBytes =
            Bytes.fromUTF8 password

        initialHash =
            HMAC.digestBytes algo passwordBytes saltBytes

        resultBytes =
            pbkdf2Loop algo passwordBytes initialHash initialHash iterations
    in
        Hex.fromByteList resultBytes


pbkdf2Loop : HMAC.Hash -> List Int -> List Int -> List Int -> Int -> List Int
pbkdf2Loop algo password hash result iterations =
    if iterations == 0 then
        result
    else
        let
            nextHash =
                HMAC.digestBytes algo password hash

            nextResult =
                List.map2 Bitwise.xor result nextHash
        in
            pbkdf2Loop algo password nextHash nextResult (iterations - 1)


{-| loop lots of times, reapplying hmac

hash = hmac(password, salt)
result = hash
for (i = 0; i < iter_count; i++) {
hash = hmac(password, hash)
result = xor(result, hash)
}
return result

-}
setPassword : User -> User
setPassword user =
    { user
        | hash = user.hash
        , salt = user.salt
    }


matchPassword : String -> String -> Bool
matchPassword p1 p2 =
    p1 == p2
