module Routes.Users exposing (..)

-- library imports

import Json.Decode as JD


-- local imports

import Types exposing (..)
import Framework.RouteParser exposing (Parser, Method(..), s, m, top, map, oneOf)
import Framework.HandlerState as HS exposing (andThen, onError, tryTask, wrapErrString, andThen2, andThen3)
import Models.User
    exposing
        ( User
        , UserId(..)
        , Username
        , Email
        , HashAndSalt
        , JwtPayload
        , decodeEmail
        , findByUsername
        , decodeUsername
        , decodeHashAndSalt
        , authObj
        , profileObj
        , verifyJWT
        )


type UsersRoute
    = Register
    | Login
    | GetCurrentUser
    | UpdateUser


urlParserUsers : Parser (UsersRoute -> parserState) parserState
urlParserUsers =
    oneOf
        [ map Register (m POST top)
        , map Login (m POST (s "login"))
        ]


urlParserUser : Parser (UsersRoute -> parserState) parserState
urlParserUser =
    oneOf
        [ map GetCurrentUser (m GET top)
        , map UpdateUser (m PUT top)
        ]


dispatch :
    Secret
    -> HandlerState EndpointError Username
    -> Connection
    -> UsersRoute
    -> EndpointState
dispatch secret authUsername conn route =
    let
        method =
            conn.request.method

        methodNotAllowedError =
            HandlerError { status = MethodNotAllowed, messages = [] }
    in
        case route of
            Register ->
                register secret conn

            Login ->
                login secret conn

            GetCurrentUser ->
                getCurrentUser secret authUsername conn

            UpdateUser ->
                updateUser secret authUsername conn


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
                |> HS.map (JD.decodeString decodeRegistrationForm)
                |> onError (wrapErrString UnprocessableEntity)

        hashAndSalt =
            formData
                |> andThen
                    (\form ->
                        AwaitingPort (HashPassword form.password) HandlerData
                            |> HS.map (JD.decodeValue decodeHashAndSalt)
                            |> onError (wrapErrString InternalError)
                    )

        createUser formData hashAndSalt =
            HandlerData
                { id = UnsavedUserId
                , username = formData.username
                , email = formData.email
                , bio = ""
                , image = Nothing
                , hash = hashAndSalt.hash
                , salt = hashAndSalt.salt
                }
    in
        andThen2 createUser formData hashAndSalt
            |> andThen (saveUser secret conn)


saveUser : Secret -> Connection -> User -> EndpointState
saveUser secret conn user =
    Models.User.save user
        |> HS.map (authObj secret conn.timestamp)


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
                |> HS.map (JD.decodeString decodeLoginForm)
                |> onError (wrapErrString UnprocessableEntity)

        user =
            formData
                |> HS.map .email
                |> andThen Models.User.findByEmail

        passwordIsValid formData user =
            AwaitingPort
                (CheckPassword
                    { hash = user.hash
                    , salt = user.salt
                    , plainText = formData.password
                    }
                )
                HandlerData
                |> HS.map (JD.decodeValue (JD.field "passwordIsValid" JD.bool))
                |> onError (wrapErrString InternalError)

        generateResponse user isValid =
            if isValid then
                HandlerData <| authObj secret conn.timestamp user
            else
                HandlerError
                    { status = Unauthorized
                    , messages = [ "Wrong username or password" ]
                    }
    in
        andThen2 passwordIsValid formData user
            |> andThen2 generateResponse user


getCurrentUser : Secret -> HandlerState EndpointError Username -> Connection -> EndpointState
getCurrentUser secret authUsername conn =
    authUsername
        |> andThen findByUsername
        |> HS.map (authObj secret conn.timestamp)


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


updateUser : Secret -> HandlerState EndpointError Username -> Connection -> EndpointState
updateUser secret authUsername conn =
    let
        formUser =
            HandlerData conn.request.body
                |> HS.map (JD.decodeString putUserFormDecoder)
                |> onError (wrapErrString UnprocessableEntity)

        dbUser =
            authUsername |> andThen findByUsername

        getHashAndSalt formUser dbUser =
            case formUser.password of
                Nothing ->
                    HandlerData
                        { hash = dbUser.hash, salt = dbUser.salt }

                Just plainText ->
                    AwaitingPort (HashPassword plainText) HandlerData
                        |> HS.map (JD.decodeValue decodeHashAndSalt)
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
        andThen2 getHashAndSalt formUser dbUser
            |> andThen3 mergeUserData formUser dbUser
            |> andThen (saveUser secret conn)
