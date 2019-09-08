DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS wallets;

CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username TEXT,
  password TEXT,
  mail TEXT
  );

CREATE TABLE wallets (
  id INTEGER PRIMARY KEY,
  keeperId INTEGER,
  walletType TEXT,
  amountM INTEGER
  );
