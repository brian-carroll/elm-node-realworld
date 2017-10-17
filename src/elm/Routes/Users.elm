module Routes.Users exposing (..)

import UrlParser exposing (Parser, s, top, map, oneOf)
import Types exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Models.User exposing (..)
import Task exposing (Task)
import Database exposing (..)
import Dict


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


dispatch : ProgramConfig -> Connection -> UsersRoute -> HandlerState
dispatch config conn route =
    let
        method =
            conn.request.method
    in
        case route of
            Register ->
                case method of
                    Post ->
                        register config.secret conn

                    _ ->
                        HandlerError MethodNotAllowed []

            Login ->
                case method of
                    Post ->
                        login config.secret conn

                    _ ->
                        HandlerError MethodNotAllowed []

            CurrentUser ->
                case method of
                    Get ->
                        getCurrentUser config.secret conn

                    Put ->
                        HandlerError InternalError []

                    _ ->
                        HandlerError MethodNotAllowed []


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


register : Secret -> Connection -> HandlerState
register secret conn =
    JD.decodeString decodeRegistrationForm conn.request.body
        |> catchError UnprocessableEntity
            (\regFormData ->
                AwaitingPort
                    (HashPassword regFormData.user.password)
                    (\connection jsHashAndSalt ->
                        JD.decodeValue decodeHashAndSalt jsHashAndSalt
                            |> catchError InternalError
                                (saveNewUser secret regFormData connection)
                    )
            )


saveNewUser : Secret -> RegistrationForm -> Connection -> HashAndSalt -> HandlerState
saveNewUser secret regFormData conn hashAndSalt =
    let
        user =
            { rev = Nothing
            , username = regFormData.user.username
            , email = regFormData.user.email
            , bio = ""
            , image = ""
            , hash = hashAndSalt.hash
            , salt = hashAndSalt.salt
            }

        successJson =
            JE.object
                [ ( "user", toAuthJSON secret conn.timestamp user ) ]

        dbTask =
            user
                |> Models.User.save
                |> Task.andThen (Task.succeed << onSaveUserSuccess successJson)
                |> Task.onError (Task.succeed << handleDbError)
    in
        AwaitingTask dbTask


onSaveUserSuccess : JE.Value -> DbPostBulkResponse -> HandlerState
onSaveUserSuccess successJson dbResponse =
    if List.all .ok dbResponse then
        HandlerSuccess successJson
    else
        HandlerError InternalError <|
            List.filterMap .error dbResponse


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


login : Secret -> Connection -> HandlerState
login secret conn =
    let
        _ =
            Debug.log "Executing handler" "login"

        validateInput : HandlerState
        validateInput =
            conn.request.body
                |> JD.decodeString decodeLoginForm
                |> catchError UnprocessableEntity
                    (\formData -> fetchUserFromDb formData)

        fetchUserFromDb : LoginForm -> HandlerState
        fetchUserFromDb formData =
            AwaitingTask
                (Models.User.findByEmail (formData.user.email)
                    |> Task.andThen (Task.succeed << checkPassword formData.user.password)
                    |> Task.onError (Task.succeed << handleDbError)
                )

        checkPassword : String -> User -> HandlerState
        checkPassword formPassword user =
            AwaitingPort
                (CheckPassword { hash = user.hash, salt = user.salt, plainText = formPassword })
                (loginResponse user)

        loginResponse : User -> Connection -> JD.Value -> HandlerState
        loginResponse user conn jsData =
            JD.decodeValue (JD.field "passwordIsValid" JD.bool) jsData
                |> catchError InternalError
                    (\isValid ->
                        if isValid then
                            HandlerSuccess <|
                                JE.object
                                    [ ( "user", toAuthJSON secret conn.timestamp user ) ]
                        else
                            HandlerError Unauthorized [ "Wrong username or password" ]
                    )
    in
        validateInput


requireAuth : Secret -> Connection -> Result String JwtPayload
requireAuth secret conn =
    case Dict.get "authorization" conn.request.headers of
        Just auth ->
            case String.words auth of
                [ "Token", token ] ->
                    verifyJWT secret conn.timestamp token

                _ ->
                    Err "Invalid token"

        Nothing ->
            let
                headersJsonString =
                    conn.request.headers
                        |> Dict.map (\k v -> JE.string v)
                        |> Dict.toList
                        |> JE.object
                        |> JE.encode 2
            in
                Err ("Authorization token not found in " ++ headersJsonString)


getCurrentUser : Secret -> Connection -> HandlerState
getCurrentUser secret conn =
    let
        validateInput : HandlerState
        validateInput =
            requireAuth secret conn
                |> catchError Unauthorized
                    (\jwtPayload -> getUserDoc jwtPayload.username)

        getUserDoc : Username -> HandlerState
        getUserDoc username =
            AwaitingTask
                (findByUsername username
                    |> Task.andThen (Task.succeed << generateResponse)
                    |> Task.onError (Task.succeed << handleDbError)
                )

        generateResponse : User -> HandlerState
        generateResponse user =
            HandlerSuccess <|
                JE.object <|
                    [ ( "user"
                      , toAuthJSON secret conn.timestamp user
                      )
                    ]
    in
        validateInput


catchError : ErrorCode -> (a -> HandlerState) -> Result String a -> HandlerState
catchError errorCode onSuccess result =
    case result of
        Ok data ->
            onSuccess data

        Err str ->
            HandlerError errorCode [ str ]
