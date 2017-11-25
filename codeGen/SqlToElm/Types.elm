module SqlToElm.Types exposing (..)


type alias File =
    { path : String
    , body : String
    }


type alias ElmFunctionHeader =
    { sqlName : String
    , elmName : String
    , args : List FunctionArg
    , returnType : String
    }


type alias SqlFile =
    { path : String
    , functions : List SqlFunction
    }


type alias FunctionArg =
    ( String, ArgType )


type alias SqlFunction =
    { name : String
    , args : List FunctionArg
    , returnType : ReturnType
    }


type ArgType
    = SqlInt
    | SqlText


type ReturnType
    = ReturnSetOf String
    | Return String
