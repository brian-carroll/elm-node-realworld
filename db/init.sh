createuser -P -d conduit
psql -U postgres -f init.sql
psql -U conduit -d conduit -f schema.sql
