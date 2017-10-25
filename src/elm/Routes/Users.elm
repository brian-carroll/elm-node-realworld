module Routes.Users exposing (..)

-- library imports

import Json.Decode as JD
import Json.Encode as JE
import Dict
import UrlParser exposing (Parser, s, top, map, oneOf)


-- local imports

import Types exposing (..)
import HandlerState exposing (andThen, onError, tryTask, wrapErrString, map2, map3)
import Models.User
    exposing
        ( User
        , Username
        , Email
        , HashAndSalt
        , decodeEmail
        , findByUsername
        , decodeUsername
        , decodeHashAndSalt
        , toAuthJSON
        , verifyJWT
        , JwtPayload
        )


type UsersRoute
    = Register
    | Login
    | CurrentUser


urlParserUsers : Parser (UsersRoute -> parserState) parserState
urlParserUsers =
    oneOf
        [ map Register top
        , map Login (s "login")
        ]


urlParserUser : Parser (UsersRoute -> parserState) parserState
urlParserUser =
    map CurrentUser top


dispatch : ProgramConfig -> Connection -> UsersRoute -> EndpointState
dispatch config conn route =
    let
        method =
            conn.request.method

        methodNotAllowedError =
            HandlerError { status = MethodNotAllowed, messages = [] }
    in
        case route of
            Register ->
                case method of
                    Post ->
                        register config.secret conn

                    _ ->
                        methodNotAllowedError

            Login ->
                case method of
                    Post ->
                        login config.secret conn

                    _ ->
                        methodNotAllowedError

            CurrentUser ->
                case method of
                    Get ->
                        getCurrentUser config.secret conn

                    Put ->
                        putCurrentUser config.secret conn

                    _ ->
                        methodNotAllowedError


type alias RegistrationForm =
    { username : Username
    , email : Email
    , password : String
    }


decodeRegistrationForm : JD.Decoder RegistrationForm
decodeRegistrationForm =
    JD.field "user" <|
        JD.map3
            RegistrationForm
            (JD.field "username" decodeUsername)
            (JD.field "email" decodeEmail)
            (JD.field "password" JD.string)


register : Secret -> Connection -> EndpointState
register secret conn =
    let
        formData =
            HandlerData conn.request.body
                |> andThen (JD.decodeString decodeRegistrationForm >> HandlerData)
                |> onError (wrapErrString UnprocessableEntity)

        hashAndSalt =
            formData
                |> andThen
                    (\form ->
                        AwaitingPort (HashPassword form.password) HandlerData
                            |> andThen (JD.decodeValue decodeHashAndSalt >> HandlerData)
                            |> onError (wrapErrString InternalError)
                    )

        createUser formData hashAndSalt =
            HandlerData
                { id = Nothing
                , username = formData.username
                , email = formData.email
                , bio = ""
                , image = Nothing
                , hash = hashAndSalt.hash
                , salt = hashAndSalt.salt
                }
    in
        map2 createUser formData hashAndSalt
            |> andThen (saveUser secret conn)


saveUser : Secret -> Connection -> User -> EndpointState
saveUser secret conn user =
    Models.User.save user
        |> andThen
            (\user ->
                HandlerData <|
                    JE.object
                        [ ( "user", toAuthJSON secret conn.timestamp user ) ]
            )


type alias LoginForm =
    { email : Email
    , password : String
    }


decodeLoginForm : JD.Decoder LoginForm
decodeLoginForm =
    JD.field "user" <|
        JD.map2
            LoginForm
            (JD.field "email" Models.User.decodeEmail)
            (JD.field "password" JD.string)


login : Secret -> Connection -> EndpointState
login secret conn =
    let
        formData =
            HandlerData conn.request.body
                |> andThen (JD.decodeString decodeLoginForm >> HandlerData)
                |> onError (wrapErrString UnprocessableEntity)

        user =
            formData
                |> andThen (.email >> HandlerData)
                |> andThen Models.User.findByEmail

        passwordIsValid formData user =
            AwaitingPort
                (CheckPassword
                    { hash = user.hash
                    , salt = user.salt
                    , plainText = formData.password
                    }
                )
                (JD.decodeValue (JD.field "passwordIsValid" JD.bool) >> HandlerData)
                |> onError (wrapErrString InternalError)

        generateResponse user isValid =
            if isValid then
                HandlerData <|
                    JE.object
                        [ ( "user", toAuthJSON secret conn.timestamp user ) ]
            else
                HandlerError
                    { status = Unauthorized
                    , messages = [ "Wrong username or password" ]
                    }
    in
        map2 passwordIsValid formData user
            |> map2 generateResponse user


requireAuth : Secret -> Connection -> HandlerState EndpointError JwtPayload
requireAuth secret conn =
    case Dict.get "authorization" conn.request.headers of
        Nothing ->
            wrapErrString Unauthorized "Authorization header required"

        Just auth ->
            case String.words auth of
                [ "Token", token ] ->
                    HandlerData token
                        |> andThen (verifyJWT secret conn.timestamp >> HandlerData)
                        |> onError (wrapErrString Unauthorized)

                _ ->
                    wrapErrString Unauthorized "Invalid token"


getCurrentUser : Secret -> Connection -> EndpointState
getCurrentUser secret conn =
    requireAuth secret conn
        |> andThen (.username >> HandlerData)
        |> andThen findByUsername
        |> andThen (HandlerData << toAuthJSON secret conn.timestamp)
        |> andThen (\userJson -> HandlerData <| JE.object <| [ ( "user", userJson ) ])


type alias PutUserForm =
    { username : Maybe Username
    , email : Maybe Email
    , bio : Maybe String
    , image : Maybe String
    , password : Maybe String
    }


putUserFormDecoder : JD.Decoder PutUserForm
putUserFormDecoder =
    JD.field "user" <|
        JD.map5 PutUserForm
            (JD.maybe (JD.field "username" decodeUsername))
            (JD.maybe (JD.field "email" decodeEmail))
            (JD.maybe (JD.field "bio" JD.string))
            (JD.maybe (JD.field "image" JD.string))
            (JD.maybe (JD.field "password" JD.string))


putCurrentUser : Secret -> Connection -> EndpointState
putCurrentUser secret conn =
    let
        username =
            requireAuth secret conn
                |> andThen (.username >> HandlerData)

        formUser =
            HandlerData conn.request.body
                |> andThen (JD.decodeString putUserFormDecoder >> HandlerData)
                |> onError (wrapErrString UnprocessableEntity)

        dbUser =
            username |> andThen findByUsername

        getHashAndSalt formUser dbUser =
            case formUser.password of
                Nothing ->
                    HandlerData
                        { hash = dbUser.hash, salt = dbUser.salt }

                Just plainText ->
                    AwaitingPort (HashPassword plainText) HandlerData
                        |> andThen (JD.decodeValue decodeHashAndSalt >> HandlerData)
                        |> onError (wrapErrString InternalError)

        mergeUserData : PutUserForm -> User -> HashAndSalt -> HandlerState x User
        mergeUserData formUser dbUser hs =
            HandlerData
                { dbUser
                    | username = Maybe.withDefault dbUser.username formUser.username
                    , email = Maybe.withDefault dbUser.email formUser.email
                    , bio = Maybe.withDefault dbUser.bio formUser.bio
                    , hash = hs.hash
                    , salt = hs.salt
                    , image =
                        case formUser.image of
                            Nothing ->
                                dbUser.image

                            Just _ ->
                                formUser.image
                }
    in
        map2 getHashAndSalt formUser dbUser
            |> map3 mergeUserData formUser dbUser
            |> andThen (saveUser secret conn)
