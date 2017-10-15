module Routes.Users exposing (..)

import UrlParser exposing (Parser, s, top, map, oneOf)
import Types exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Models.User exposing (..)
import Http
import Task
import Time exposing (Time)
import Database exposing (..)
import Auth exposing (HashAndSalt, decodeHashAndSalt)


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


dispatch : Method -> UsersRoute -> Connection -> HandlerState
dispatch method route conn =
    case route of
        Register ->
            HandlerSuccess JE.null

        _ ->
            HandlerSuccess JE.null


type alias RegistrationFormDetails =
    { username : Username
    , email : Email
    , password : String
    }


type alias RegistrationForm =
    { user : RegistrationFormDetails }


decodeCreateUserInputData : JD.Decoder RegistrationForm
decodeCreateUserInputData =
    JD.map RegistrationForm <|
        JD.field "user" <|
            JD.map3
                RegistrationFormDetails
                (JD.field "username" decodeUsername)
                (JD.field "email" decodeEmail)
                (JD.field "password" JD.string)


createUser : RegistrationFormDetails -> HashAndSalt -> User
createUser formDetails hashAndSalt =
    { id = Nothing
    , username = formDetails.username
    , email = formDetails.email
    , bio = ""
    , image = ""
    , hash = hashAndSalt.hash
    , salt = hashAndSalt.salt
    }


registerUser : Secret -> Connection -> HandlerState
registerUser secret conn =
    let
        inputResult =
            Debug.log "step 1, decoding input JSON" <|
                JD.decodeString
                    decodeCreateUserInputData
                    conn.request.body
    in
        case inputResult of
            Err str ->
                HandlerError Types.BadRequest str

            Ok regFormData ->
                AwaitingPort
                    (HashPassword regFormData.user.password)
                    (registerUserStep2 secret regFormData)


registerUserStep2 : Secret -> RegistrationForm -> Connection -> JD.Value -> HandlerState
registerUserStep2 secret regFormData conn jsHashAndSalt =
    case JD.decodeValue decodeHashAndSalt jsHashAndSalt of
        Ok hashAndSalt ->
            registerUserStep3 secret regFormData conn hashAndSalt

        Err e ->
            HandlerError InternalError e



-- TODO: generalise the above function to map Result to HandlerState


registerUserStep3 : Secret -> RegistrationForm -> Connection -> HashAndSalt -> HandlerState
registerUserStep3 secret regFormData conn hashAndSalt =
    let
        user =
            createUser
                regFormData.user
                hashAndSalt

        now =
            Tuple.first conn.id

        dbTask =
            user
                |> toDatabaseJSON
                |> createDbDoc
                |> Http.toTask
                |> Task.andThen (Task.succeed << handleDbUserCreatedSuccess secret now user)
                |> Task.onError (Task.succeed << handleDbError)
    in
        AwaitingTask dbTask


handleDbUserCreatedSuccess : Secret -> Time -> User -> DbCreateDocResponse -> HandlerState
handleDbUserCreatedSuccess secret now user dbResponse =
    case toAuthJSON secret now { user | id = Just dbResponse.id } of
        Just json ->
            HandlerSuccess json

        Nothing ->
            HandlerError InternalError "DB user created but has no ID"
