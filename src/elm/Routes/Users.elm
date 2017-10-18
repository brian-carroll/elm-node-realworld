module Routes.Users exposing (..)

import UrlParser exposing (Parser, s, top, map, oneOf)
import Types exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Models.User exposing (..)
import Database exposing (..)
import Dict
import HandlerState exposing (andThen, onError, tryTask, wrapErrString)


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


type alias RegistrationFormUser =
    { username : Username
    , email : Email
    , password : String
    }


type alias RegistrationForm =
    { user : RegistrationFormUser }


decodeRegistrationForm : JD.Decoder RegistrationForm
decodeRegistrationForm =
    JD.map RegistrationForm <|
        JD.field "user" <|
            JD.map3
                RegistrationFormUser
                (JD.field "username" decodeUsername)
                (JD.field "email" decodeEmail)
                (JD.field "password" JD.string)


register : Secret -> Connection -> EndpointState
register secret conn =
    HandlerData conn.request.body
        |> andThen (JD.decodeString decodeRegistrationForm >> HandlerData)
        |> onError (wrapErrString UnprocessableEntity)
        |> andThen
            (\formData ->
                AwaitingPort (HashPassword formData.user.password) HandlerData
                    |> andThen (JD.decodeValue decodeHashAndSalt >> HandlerData)
                    |> onError (wrapErrString InternalError)
                    |> andThen
                        (\hashAndSalt ->
                            saveUser
                                secret
                                conn
                                { rev = Nothing
                                , username = formData.user.username
                                , email = formData.user.email
                                , bio = ""
                                , image = ""
                                , hash = hashAndSalt.hash
                                , salt = hashAndSalt.salt
                                }
                        )
            )


saveUser : Secret -> Connection -> User -> EndpointState
saveUser secret conn user =
    HandlerData user
        |> tryTask handleDbError Models.User.save
        |> andThen
            (\dbResponse ->
                if List.all .ok dbResponse then
                    HandlerData <|
                        JE.object
                            [ ( "user", toAuthJSON secret conn.timestamp user ) ]
                else
                    HandlerError
                        { status = InternalError
                        , messages = List.filterMap .error dbResponse
                        }
            )


type alias LoginForm =
    { user : LoginFormUser
    }


type alias LoginFormUser =
    { email : Email
    , password : String
    }


decodeLoginForm : JD.Decoder LoginForm
decodeLoginForm =
    JD.map LoginForm
        (JD.field "user" decodeLoginFormUser)


decodeLoginFormUser : JD.Decoder LoginFormUser
decodeLoginFormUser =
    JD.map2 LoginFormUser
        (JD.field "email" Models.User.decodeEmail)
        (JD.field "password" JD.string)


login : Secret -> Connection -> EndpointState
login secret conn =
    HandlerData conn.request.body
        |> andThen (JD.decodeString decodeLoginForm >> HandlerData)
        |> onError (wrapErrString UnprocessableEntity)
        |> andThen
            (\formData ->
                HandlerData formData.user.email
                    |> tryTask handleDbError Models.User.findByEmail
                    |> andThen
                        (\user ->
                            AwaitingPort
                                (CheckPassword
                                    { hash = user.hash
                                    , salt = user.salt
                                    , plainText = formData.user.password
                                    }
                                )
                                HandlerData
                                |> andThen (JD.decodeValue (JD.field "passwordIsValid" JD.bool) >> HandlerData)
                                |> onError (wrapErrString InternalError)
                                |> andThen
                                    (\isValid ->
                                        if isValid then
                                            HandlerData <|
                                                JE.object
                                                    [ ( "user", toAuthJSON secret conn.timestamp user ) ]
                                        else
                                            HandlerError
                                                { status = Unauthorized
                                                , messages = [ "Wrong username or password" ]
                                                }
                                    )
                        )
            )


requireAuth : Secret -> Connection -> HandlerState EndpointError JwtPayload
requireAuth secret conn =
    case Dict.get "authorization" conn.request.headers of
        Just auth ->
            case String.words auth of
                [ "Token", token ] ->
                    HandlerData token
                        |> andThen (verifyJWT secret conn.timestamp >> HandlerData)
                        |> onError (wrapErrString Unauthorized)

                _ ->
                    wrapErrString Unauthorized "Invalid token"

        Nothing ->
            wrapErrString Unauthorized "Authorization token not found"


getCurrentUser : Secret -> Connection -> EndpointState
getCurrentUser secret conn =
    requireAuth secret conn
        |> andThen (.username >> HandlerData)
        |> tryTask handleDbError findByUsername
        |> andThen
            (\user ->
                HandlerData <|
                    JE.object <|
                        [ ( "user"
                          , toAuthJSON secret conn.timestamp user
                          )
                        ]
            )


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
    requireAuth secret conn
        |> andThen (.username >> HandlerData)
        |> andThen
            (\username ->
                HandlerData conn.request.body
                    |> andThen (JD.decodeString putUserFormDecoder >> HandlerData)
                    |> onError (wrapErrString UnprocessableEntity)
                    |> andThen
                        (\formUser ->
                            HandlerData username
                                |> tryTask handleDbError findByUsername
                                |> andThen (mergeUserData secret formUser)
                                |> andThen (saveUser secret conn)
                        )
            )


mergeUserData : Secret -> PutUserForm -> User -> HandlerState EndpointError User
mergeUserData secret formUser dbUser =
    let
        hashAndSalt =
            case formUser.password of
                Nothing ->
                    HandlerData
                        { hash = dbUser.hash, salt = dbUser.salt }

                Just plainText ->
                    AwaitingPort (HashPassword plainText) HandlerData
                        |> andThen (JD.decodeValue decodeHashAndSalt >> HandlerData)
                        |> onError (wrapErrString InternalError)
    in
        hashAndSalt
            |> andThen
                (\hs ->
                    HandlerData
                        { dbUser
                            | username = Maybe.withDefault dbUser.username formUser.username
                            , email = Maybe.withDefault dbUser.email formUser.email
                            , bio = Maybe.withDefault dbUser.bio formUser.bio
                            , image = Maybe.withDefault dbUser.image formUser.image
                            , hash = hs.hash
                            , salt = hs.salt
                        }
                )
