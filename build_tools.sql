PRAGMA foreign_keys = ON;
CREATE TABLE IF NOT EXISTS users (
  username TEXT PRIMARY KEY,
  password TEXT,
  mail TEXT,
  UNIQUE (username)
);
CREATE TABLE IF NOT EXISTS wallets (
  id INTEGER PRIMARY KEY,
  publicId TEXT,
  keeperName TEXT,
  walletType TEXT,
  amountM INTEGER,
  FOREIGN KEY (keeperName) REFERENCES users(username)
);
CREATE TABLE IF NOT EXISTS logins (
  id INTEGER PRIMARY KEY,
  keeperName TEXT,
  ip TEXT,
  FOREIGN KEY (keeperName) REFERENCES users(username)
);
CREATE TABLE IF NOT EXISTS history (
  id INTEGER PRIMARY KEY,
  walletFrom INTEGER,
  walletTo INTEGER,
  description TEXT,
  sqltime TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  FOREIGN KEY (walletFrom) REFERENCES wallets(id),
  FOREIGN KEY (walletTo) REFERENCES wallets(id)
);
