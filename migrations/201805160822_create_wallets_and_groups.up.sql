-- Wallets

CREATE TABLE wallets (
       id            serial  PRIMARY KEY,
       name          varchar CHECK (name <> ''),
       description   varchar CHECK (description <> ''),
       credits       integer NOT NULL DEFAULT 0,
       creation_time timestamp with time zone NOT NULL DEFAULT now(),
       deletion_time timestamp with time zone,
       CONSTRAINT data_not_null_xor_deleted CHECK
         ((deletion_time IS NOT NULL AND name IS NULL AND description IS NULL)
          OR (deletion_time IS NULL AND name IS NOT NULL))
);

CREATE TABLE wallet_memberships (
       user_id   integer NOT NULL REFERENCES users (id),
       wallet_id integer NOT NULL REFERENCES wallets (id),
       UNIQUE (user_id, wallet_id)
);

CREATE TRIGGER deletion_forbidden_on_wallets BEFORE DELETE ON wallets
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('wallets');
CREATE TRIGGER truncation_forbidden_on_wallets BEFORE TRUNCATE ON wallets
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('wallets');

CREATE VIEW active_wallets AS
       SELECT id, name, description, credits, creation_time
       FROM wallets WHERE deletion_time IS NULL;

CREATE RULE delete_from_active_wallets AS
       ON DELETE TO active_wallets DO INSTEAD
       UPDATE wallets
       SET deletion_time = now(),
           name          = NULL,
           description   = NULL
       WHERE id = OLD.id AND deletion_time IS NULL;

-- Groups

CREATE TABLE groups (
       id            serial  PRIMARY KEY,
       name          varchar CHECK (name <> ''),
       description   varchar CHECK (description <> ''),
       wallet_id     integer REFERENCES wallets (id),
       creation_time timestamp with time zone NOT NULL DEFAULT now(),
       deletion_time timestamp with time zone,
       CONSTRAINT data_not_null_xor_deleted CHECK
         ((deletion_time IS NOT NULL AND name IS NULL AND description IS NULL)
          OR (deletion_time IS NULL AND name IS NOT NULL))
);

CREATE TABLE group_memberships (
       user_id  integer NOT NULL REFERENCES users (id),
       group_id integer NOT NULL REFERENCES groups (id),
       UNIQUE (user_id, group_id)
);

CREATE TRIGGER deletion_forbidden_on_groups BEFORE DELETE ON groups
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('groups');
CREATE TRIGGER truncation_forbidden_on_groups BEFORE TRUNCATE ON groups
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('groups');

CREATE VIEW active_groups AS
       SELECT id, name, description, wallet_id, creation_time
       FROM groups WHERE deletion_time IS NULL;

CREATE RULE delete_from_active_groups AS
       ON DELETE TO active_groups DO INSTEAD
       UPDATE groups
       SET deletion_time = now(),
           name          = NULL,
           description   = NULL
       WHERE id = OLD.id AND deletion_time IS NULL;

CREATE RULE delete_wallet_id_from_groups AS
       ON UPDATE TO wallets
       WHERE NEW.deletion_time IS NOT NULL
       DO ALSO
       UPDATE groups SET wallet_id = NULL WHERE wallet_id = NEW.id;
