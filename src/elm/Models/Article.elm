module Article exposing (..)


type Slug
    = Slug String


type alias Article =
    { slug : Slug
    , title : String
    , description : String
    , body : String
    }



{-

   create table if not exists articles
       ( id serial primary key
       , author_id int not null references users
       , slug text not null unique
       , title text not null
       , description text not null
       , body text not null
       , created_at timestamptz default now()
       , updated_at timestamptz default now()
       );
-}
