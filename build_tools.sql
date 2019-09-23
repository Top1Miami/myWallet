DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS wallets;
DROP TABLE IF EXISTS logins;

CREATE TABLE users (
  username TEXT PRIMARY KEY,
  password TEXT,
  mail TEXT,
  UNIQUE (username)
);
CREATE TABLE wallets (
  id INTEGER PRIMARY KEY,
  publicId TEXT,
  keeperName TEXT,
  walletType TEXT,
  amountM INTEGER,
  FOREIGN KEY (keeperName) REFERENCES users(username)
);
CREATE TABLE logins (
  id INTEGER PRIMARY KEY,
  keeperName TEXT,
  ip TEXT,
  FOREIGN KEY (keeperName) REFERENCES users(username)
);
CREATE TABLE history (
  id INTEGER PRIMARY KEY,
  walletFrom INTEGER,
  walletTo INTEGER,
  description TEXT,
  FOREIGN KEY (walletFrom) REFERENCES wallets(id),
  FOREIGN KEY (walletTo) REFERENCES wallets(id)
);
