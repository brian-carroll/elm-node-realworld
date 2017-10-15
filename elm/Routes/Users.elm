module Routes.Users exposing (..)

import UrlParser exposing (Parser, s, top, map, oneOf)
import Types exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Models.User exposing (..)
import Task
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


dispatch : ProgramConfig -> Connection -> UsersRoute -> HandlerState
dispatch config conn route =
    case ( route, conn.request.method ) of
        ( Register, Post ) ->
            register config.secret conn

        _ ->
            HandlerSuccess JE.null


type alias RegistrationFormDetails =
    { username : Username
    , email : Email
    , password : String
    }


type alias RegistrationForm =
    { user : RegistrationFormDetails }


catchError : HttpStatus -> (a -> HandlerState) -> Result String a -> HandlerState
catchError errorType onSuccess result =
    case result of
        Ok data ->
            onSuccess data

        Err str ->
            HandlerError errorType str


decodeRegistrationForm : JD.Decoder RegistrationForm
decodeRegistrationForm =
    JD.map RegistrationForm <|
        JD.field "user" <|
            JD.map3
                RegistrationFormDetails
                (JD.field "username" decodeUsername)
                (JD.field "email" decodeEmail)
                (JD.field "password" JD.string)


register : Secret -> Connection -> HandlerState
register secret conn =
    JD.decodeString decodeRegistrationForm conn.request.body
        |> catchError Types.BadRequest
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

        now =
            Tuple.first conn.id

        dbTask =
            user
                |> Models.User.save
                |> Task.andThen (Task.succeed << onSaveUserSuccess (toAuthJSON secret now user))
                |> Task.onError (Task.succeed << handleDbError)
    in
        AwaitingTask dbTask


onSaveUserSuccess : JE.Value -> DbPostBulkResponse -> HandlerState
onSaveUserSuccess successJson dbResponse =
    if List.all .ok dbResponse then
        HandlerSuccess successJson
    else
        HandlerError InternalError <|
            """ {"dbErrors": ["""
                ++ (String.join ", " <| List.filterMap .error dbResponse)
                ++ """ ]} """
