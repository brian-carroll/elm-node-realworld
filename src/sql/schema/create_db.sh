createuser -P -d conduit
psql -U postgres -f create_db.sql
psql -U conduit -d conduit -f schema.sql
