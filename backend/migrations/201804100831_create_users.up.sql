CREATE TABLE users (
       id            serial  PRIMARY KEY,
       email         varchar UNIQUE CHECK (email <> ''),
       name          varchar CHECK (name <> ''),
       password_hash bytea   NOT NULL CHECK (password_hash <> ''),
       creation_time timestamp with time zone NOT NULL DEFAULT now(),
       deletion_time timestamp with time zone,
       CONSTRAINT data_not_null_xor_deleted CHECK
         ((deletion_time IS NOT NULL AND email IS NULL AND name IS NULL)
          OR (deletion_time IS NULL AND email IS NOT NULL AND name IS NOT NULL))
);

CREATE FUNCTION deletion_forbidden() RETURNS trigger AS $$
BEGIN
  RAISE 'No deletion permitted on table %', TG_ARGV[0];
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER deletion_forbidden_on_users BEFORE DELETE ON users
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('users');
CREATE TRIGGER truncation_forbidden_on_users BEFORE TRUNCATE ON users
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('users');

CREATE VIEW active_users AS
       SELECT id, email, name, password_hash, creation_time
       FROM users WHERE deletion_time IS NULL;

CREATE RULE delete_from_active_users AS
       ON DELETE TO active_users DO INSTEAD
       UPDATE users
       SET deletion_time = now(),
           email = NULL,
           name = NULL,
           password_hash = 'deleted'
       WHERE id = OLD.id AND deletion_time IS NULL;
