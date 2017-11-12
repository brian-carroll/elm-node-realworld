create or replace function trigger_set_timestamp()
    returns trigger as $$
    begin
    new.updated_at = now();
    return new;
    end;
$$ language plpgsql;


create table if not exists users
    ( id serial primary key
    , username text not null
    , email text not null
    , bio text not null
    , image text
    , hash text not null
    , salt text not null
    );
create unique index if not exists
    users_unique_lower_username_idx
    on users (lower(username));
create unique index if not exists
    users_unique_lower_email_idx
    on users (lower(email));


create table if not exists follows
    ( follower_id int not null references users on update cascade on delete cascade
    , followed_id int not null references users on update cascade on delete cascade
    , constraint follows_pk primary key (follower_id, followed_id)
    );


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
create trigger set_timestamp
    before update on articles
    for each row
    execute procedure trigger_set_timestamp();


create table if not exists comments
    ( id serial primary key
    , user_id int not null references users
    , article_id int not null references articles
    , body text not null
    , created_at timestamptz default now()
    , updated_at timestamptz default now()
    );
create trigger set_timestamp
    before update on comments
    for each row
    execute procedure trigger_set_timestamp();


create table if not exists favourites
    ( user_id int not null references users on update cascade on delete cascade
    , article_id int not null references articles on update cascade on delete cascade
    , constraint favourites_pk primary key (user_id, article_id)
    );


create table if not exists tags
    ( id serial primary key
    , tag text not null unique
    );


create table if not exists article_tags
    ( tag_id int not null references tags on update cascade on delete cascade
    , article_id int not null references articles on update cascade on delete cascade
    , constraint article_tags_pk primary key (tag_id, article_id)
    );
