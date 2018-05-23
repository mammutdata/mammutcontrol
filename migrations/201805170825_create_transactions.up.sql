CREATE TABLE transactions (
       id            serial  PRIMARY KEY,
       user_id       integer REFERENCES users (id),
       description   varchar NOT NULL CHECK (description <> ''),
       creation_time timestamp with time zone NOT NULL DEFAULT now()
);

CREATE TYPE account_type AS ENUM ('wallet', 'replica', 'bank', 'profit');

CREATE TABLE transaction_lines (
       id              serial  PRIMARY KEY,
       transaction_id  integer NOT NULL REFERENCES transactions (id),
       account_type    account_type NOT NULL,
       wallet_id       integer REFERENCES wallets (id),
       replica_id      integer REFERENCES replicas (id),
       amount          integer NOT NULL CHECK (amount <> 0),
       currency_amount double precision
);

ALTER TABLE transaction_lines
      ADD CONSTRAINT check_account_consistency
      CHECK ((account_type = 'bank' OR account_type = 'profit')
              AND wallet_id IS NULL AND replica_id IS NULL
             OR (account_type = 'wallet' AND wallet_id IS NOT NULL
                 AND replica_id IS NULL)
             OR (account_type = 'replica' AND replica_id IS NOT NULL
                 AND wallet_id IS NULL));

CREATE FUNCTION check_balanced_transaction() RETURNS trigger AS $$
DECLARE
  sum integer;
BEGIN
  SELECT SUM(amount) INTO sum FROM transaction_lines
         WHERE transaction_id = NEW.id;
  IF sum <> 0 THEN
     RAISE 'Transaction with non-zero sum (%)', sum;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_balanced_transaction_trigger
       AFTER INSERT ON transactions
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW EXECUTE PROCEDURE check_balanced_transaction();
