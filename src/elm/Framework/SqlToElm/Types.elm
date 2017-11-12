module Framework.SqlToElm.Types exposing (..)


type alias ElmFunctionHeader =
    { name : String
    , args : List ( String, ArgType )
    , returnType : String
    }


type alias SqlFunctionHeader =
    { name : String
    , args : List ( String, ArgType )
    , returnType : ReturnType
    }


type ArgType
    = SqlInt
    | SqlText


type ReturnType
    = ReturnSetOf String
    | Return String
