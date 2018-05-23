CREATE TABLE replicas (
       id          serial  PRIMARY KEY,
       group_id    integer NOT NULL REFERENCES groups (id),
       name        varchar CHECK (name <> ''),
       description varchar CHECK (description <> ''),
       wallet_id   integer REFERENCES wallets (id),
       creation_time timestamp with time zone NOT NULL DEFAULT now(),
       deletion_time timestamp with time zone,
       CONSTRAINT data_not_null_xor_deleted CHECK
         ((deletion_time IS NOT NULL AND name IS NULL AND description IS NULL)
          OR (deletion_time IS NULL AND name IS NOT NULL))
);

CREATE TRIGGER deletion_forbidden_on_replicas BEFORE DELETE ON replicas
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('replicas');
CREATE TRIGGER truncation_forbidden_on_replicas BEFORE TRUNCATE ON replicas
       FOR EACH STATEMENT EXECUTE PROCEDURE deletion_forbidden('replicas');

CREATE VIEW active_replicas AS
       SELECT id, group_id, name, description, wallet_id, creation_time
       FROM replicas WHERE deletion_time IS NULL;

CREATE RULE delete_from_active_replicas AS
       ON DELETE TO active_replicas DO INSTEAD
       UPDATE replicas
       SET deletion_time = now(),
           name          = NULL,
           description   = NULL
       WHERE id = OLD.id AND deletion_time IS NULL;

CREATE RULE delete_wallet_id_from_replicas AS
       ON UPDATE TO wallets
       WHERE NEW.deletion_time IS NOT NULL
       DO ALSO
       UPDATE replicas SET wallet_id = NULL WHERE wallet_id = NEW.id;

CREATE RULE delete_replicas_with_groups AS
       ON UPDATE TO groups
       WHERE NEW.deletion_time IS NOT NULL
       DO ALSO
       DELETE FROM active_replicas WHERE group_id = NEW.id;
