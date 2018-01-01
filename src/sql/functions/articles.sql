create or replace function list_articles(
    _tag text,
    _author text,
    _favourited text,
    _limit int default 20,
    _offset int default 0
    ) returns setof articles
as $$
    select articles.*
    from articles
        left join article_tags on article_tags.article_id=articles.id
        left join tags on article_tags.tag_id=tags.id

        left join users as authors on articles.author_id=authors.id

        left join favourites on favourites.article_id=articles.id
        left join users as users_favourites on favourites.user_id=users_favourites.id
    where
            (_tag is null or tags.tag=_tag)
        and
            (_author is null or authors.username=_author)
        and
            (_favourited is null or users_favourites.username=_favourited)
    group by articles.id
    order by articles.created_at desc
    limit _limit
    offset _offset
    ;
$$
language sql;

drop type if exists article_details;
create type article_details as 
    ( id int
    , author_id int
    , slug text
    , title text
    , description text
    , body text
    , created_at timestamptz
    , updated_at timestamptz
    , username text
    , email text
    , bio text
    , image text
    , hash text
    , salt text
    , favourites_count bigint
    , favourited boolean
    , following_author boolean
    , tag_list text[]
    );

drop function article_details_by_slug(int, text); -- in case return type has changed
create function article_details_by_slug
    ( _current_user_id int
    , _slug text
    ) returns setof article_details
as $$
    select
        articles.id,
        articles.author_id,
        articles.slug,
        articles.title,
        articles.description,
        articles.body,
        articles.created_at,
        articles.updated_at,
        authors.username,
        authors.email,
        authors.bio,
        authors.image,
        authors.hash,
        authors.salt,
        count(favourites) as favourites_count,
        sum(case when favourites.user_id=_current_user_id then 1 else 0 end)>0 as favourited,
        sum(case when follows.follower_id=_current_user_id then 1 else 0 end)>0 as following_author,
        array(
            select tags.tag from
            article_tags inner join tags on article_tags.tag_id=tags.id
            where article_tags.article_id=articles.id
        ) as tag_list
    from articles
        inner join users as authors on articles.author_id=authors.id
        left join favourites on favourites.article_id=articles.id
        left join follows on follows.followed_id=authors.id
    where
        slug=_slug
    group by
        articles.id, authors.id
    order by
        articles.created_at desc
    ;
$$
language sql;

