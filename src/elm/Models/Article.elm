module Article exposing (..)


type Slug
    = Slug String


type alias Article =
    { slug : Slug
    , title : String
    , description : String
    , body : String
    }
