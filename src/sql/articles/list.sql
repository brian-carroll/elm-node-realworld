create or replace function list(
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
