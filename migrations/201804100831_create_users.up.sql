CREATE TABLE users (
       id            serial  PRIMARY KEY,
       email         varchar NOT NULL UNIQUE CHECK (email <> ''),
       password_hash bytea   NOT NULL CHECK (password_hash <> '')
);
