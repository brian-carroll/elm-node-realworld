-- First connect to conduit database ('\c' in psql)

CREATE DATABASE conduit;

GRANT SELECT, INSERT, UPDATE, DELETE
ON ALL TABLES IN SCHEMA public 
TO conduit;

GRANT USAGE
ON ALL SEQUENCES IN SCHEMA public 
TO conduit;
