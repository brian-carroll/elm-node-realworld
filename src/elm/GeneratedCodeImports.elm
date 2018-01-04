module GeneratedCodeImports exposing (..)

{-| Imports from handwritten code to generated code

Central location for the generated code to import everything it needs from the handwritten code
This project structure makes it easier to refactor things without changing code generation

-}

import Models.Article
import Types
import Models.Utils


type alias Article =
    Models.Article.Article


type alias SqlResult a =
    Types.HandlerState Types.EndpointError a


runSqlQuery =
    Models.Utils.runSqlQuery
