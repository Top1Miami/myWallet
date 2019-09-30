{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( addWallet
  , addUser
  , transferMoneyById
  , verifyUser
  , depositWallet
  , autoLogin
  , logOut
  , logIn
  , transferMoneyBetween
  , getFixedHistory
  , getUserHistory
  ) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import Data.Maybe
import Control.Exception

data User = User { userName :: String
                 , password :: String
                 , mail :: String
                 }

data Wallet = Wallet { walletId :: Int
                     , publicId :: String
                     , keeperName :: String
                     , walletType :: String
                     , amountM :: Int 
                     }

data Login = Login { loginId :: Int
                   , loginKeeperName :: String
                   , ip :: String
                   }

data History = History { historyId :: Int
                       , idWalletFrom :: Int
                       , idWalletTo :: Int
                       , description :: String
                       , operationDate :: UTCTime
                       }

instance Show User where
  show user = mconcat [ show $ userName user
                      , ", password : "
                      , password user
                      , "\nmail : " 
                      , mail user
                      ]
instance Show Wallet where
  show wallet = mconcat [ show $ amountM wallet
                        , "|"
                        , walletType wallet
                        , "-"
                        , publicId wallet
                        ] 
instance Show History where
  show history = description history

instance FromRow History where
  fromRow = History <$> field <*> field <*> field <*> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Wallet where
  fromRow = Wallet <$> field <*> field <*> field <*> field <*> field

instance FromRow Login where
  fromRow = Login <$> field <*> field <*> field  

getPublicId :: String -> String -> String
getPublicId username walletType = walletType ++ username

addUser :: String -> String -> String -> IO (Maybe String)
addUser username password mail = do
  conn <- open "tools.db"
  (withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> do
        execute conn "INSERT INTO users (username, password, mail) VALUES (?, ?, ?)" (username, password, mail)
        return $ (Just "success")
      Just user -> do
        return Nothing) <* (close conn)    

foundAny :: [a] -> Maybe a
foundAny [] = Nothing
foundAny (x:_) = Just x 

selectUser :: Connection -> String -> IO (Maybe User)
selectUser conn username = do
  response <- query conn "SELECT * FROM users WHERE username = (?)" (Only username)
  return $ foundAny response

verifyUser :: String -> String -> Bool -> IO (Maybe String)
verifyUser username pwd needPsw= do
  conn <- open "tools.db"
  maybeUser <- selectUser conn username
  putStrLn $ show maybeUser
  case maybeUser of
    Nothing -> return Nothing
    Just user -> if ((password user) == pwd || needPsw)  
      then fmap Just $ (getAllWallets user) >>= 
        (maybe (return "NotFound") (\list -> return $ foldl (\conc arg -> conc ++ (show arg) ++ " ") "" list))
      else return $ Nothing

addWallet :: String -> String -> Int -> IO (Maybe String)
addWallet username walletType amountM = do
  conn <- open "tools.db"  
  (withExclusiveTransaction conn $ do
    maybeWallet <- selectWalletByNameType conn username walletType
    case maybeWallet of
      Nothing -> do 
        execute conn "INSERT INTO wallets (publicId, keeperName, walletType, amountM) VALUES (?, ?, ?, ?)" (getPublicId username walletType, username, walletType, amountM)
        return $ Just $ getPublicId username walletType
      Just wallet -> do
        return Nothing) <* (close conn)

selectWallet :: Connection -> User -> String -> IO (Maybe Wallet)
selectWallet conn user walletType = do
  response <- query conn "SELECT * FROM wallets WHERE keeperName = ? AND walletType = ?" (userName user, walletType)
  return $ foundAny response

selectWalletByPublicId :: Connection -> String -> IO (Maybe Wallet)
selectWalletByPublicId conn pubId = do
  response <- query conn "SELECT * FROM wallets WHERE publicId = ?" (Only pubId)
  return $ foundAny response

selectWalletById :: Connection -> Int -> IO (Maybe Wallet)
selectWalletById conn walletId = do
  response <- query conn "SELECT * FROM wallets WHERE id = ?" (Only walletId)
  return $ foundAny response

selectWalletByNameType :: Connection -> String -> String -> IO (Maybe Wallet)
selectWalletByNameType conn username walletType = do
  response <- query conn "SELECT * FROM wallets WHERE keeperName = ? AND walletType = ?" (username, walletType)
  return $ foundAny response

--TODO rewrite
foundAll :: [a] -> Maybe [a]
foundAll [] = Nothing
foundAll list = Just list 

getAllWallets :: User -> IO (Maybe [Wallet])
getAllWallets user = do
  conn <- open "tools.db"
  response <- query conn "SELECT * FROM wallets WHERE keeperName = ?" (Only $ userName user)
  close conn
  return $ foundAll $ response

data TransferException = WalletNotFoundException | NotEnoughMoneyException deriving (Show)

instance Exception TransferException where

transferMoneyById :: String -> String -> Int -> IO (Maybe String)
transferMoneyById fromId toId toTransfer = do
  conn <- open "tools.db"
  res <- (try $ withExclusiveTransaction conn $ do
    maybeFrom <- selectWalletByPublicId conn fromId
    maybeTo <- selectWalletByPublicId conn toId

    if (isNothing maybeFrom || isNothing maybeTo)
    then
      throw WalletNotFoundException
    else 
      do
        let walletFrom = fromJust maybeFrom
        let walletTo = fromJust maybeTo
        if (amountM walletFrom < toTransfer)
        then
          throw NotEnoughMoneyException
        else do
          execute conn "INSERT INTO history (walletFrom, walletTo, description) VALUES (?, ?, ?)" (walletId walletFrom, walletId walletTo, "transfer from " ++ (publicId walletFrom) ++ " to " ++ (publicId walletTo) ++ " " ++ (show toTransfer))
          onSuccess conn walletFrom walletTo toTransfer) :: IO (Either TransferException (Maybe String))
  close conn
  case res of
    Left ex -> do
      -- putStrLn $ show ex
      return $ Nothing
    Right succ -> return succ

transferMoneyBetween :: String -> String -> String -> Int -> IO (Maybe String)
transferMoneyBetween username from to toTransfer = do
  conn <- open "tools.db"
  res <- (try $ withExclusiveTransaction conn $ do
    maybeFrom <- selectWalletByNameType conn username from
    maybeTo <- selectWalletByNameType conn username to

    if (isNothing maybeFrom || isNothing maybeTo)
    then
      throw WalletNotFoundException
    else 
      do
        let walletFrom = fromJust maybeFrom
        let walletTo = fromJust maybeTo
        if (amountM walletFrom < toTransfer)
        then
          throw NotEnoughMoneyException
        else do
          execute conn "INSERT INTO history (walletFrom, walletTo, description) VALUES (?, ?, ?)" (walletId walletFrom, walletId walletTo, "transfer between your wallets: from " ++ (walletType walletFrom) ++ " to " ++ (walletType walletTo) ++ " " ++ (show toTransfer))
          onSuccess conn walletFrom walletTo toTransfer) :: IO (Either TransferException (Maybe String))
  close conn
  case res of
    Left ex -> do
      -- putStrLn $ show ex
      return $ Nothing
    Right succ -> return succ

onSuccess :: Connection -> Wallet -> Wallet -> Int -> IO (Maybe String)
onSuccess conn walletFrom walletTo money = do  
  execute conn "UPDATE wallets SET amountM = ? WHERE id = ?" (amountM walletFrom - money, walletId walletFrom)
  execute conn "UPDATE wallets SET amountM = ? WHERE id = ?" (amountM walletTo + money, walletId walletTo)
  maybeFrom <- selectWalletById conn (walletId walletFrom)
  maybeTo <- selectWalletById conn (walletId walletTo)
  return $ getWalletInfo maybeFrom maybeTo    
  where
    getWalletInfo :: Maybe Wallet -> Maybe Wallet -> Maybe String
    getWalletInfo Nothing _ = Nothing
    getWalletInfo _ Nothing = Nothing
    getWalletInfo (Just from) (Just to) = Just (show from ++ show to)

deleteUser :: String -> IO ()
deleteUser username = do
  conn <- open "tools.db"
  withExclusiveTransaction conn $ do
    execute conn "DELETE FROM wallets WHERE keeperName = ?" (Only username)  
    execute conn "DELETE FROM users WHERE username = ?" (Only username)
  close conn

deleteWallet :: String -> String -> IO ()
deleteWallet username walletType = do
  conn <- open "tools.db"
  withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> putStrLn "user not found, operation failed"
      Just user -> do 
        execute conn "DELETE FROM users WHERE keeperName = ? AND walletType = ?" (userName user, walletType)
  close conn

depositWallet :: String -> String -> String -> IO (Bool)
depositWallet username walletType amount = do
  conn <- open "tools.db"
  (withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> do 
        putStrLn "user not found, operation failed"
        return False
      Just user -> do
        execute conn "UPDATE wallets SET amountM = ? WHERE keeperName = ? AND walletType = ?" ((read amount):: Int, userName user, walletType)
        return True) <* (close conn)

autoLogin :: String -> IO (Maybe String)
autoLogin ip = do
  conn <- open "tools.db"
  putStrLn ip
  login <- query conn "SELECT * FROM logins WHERE ip = ?" (Only ip) :: IO [Login]
  putStrLn "ne run" 
  (case (foundAny login) of
    Nothing -> do 
      return $ Nothing
    Just x -> do
      userList <- query conn "SELECT * FROM users WHERE username = ?" (Only $ loginKeeperName x) :: IO [User]
      case (foundAny userList) of
        Nothing -> do
          return $ Nothing
        Just user -> do
          fmap Just $ (getAllWallets user) >>= (maybe (return "NotFound") (\list -> return $ foldl (\conc arg -> conc ++ (show arg) ++ " ") "" list)) >>= (\inp -> return $ inp ++ " " ++ (userName user))) <* (close conn)

logOut :: String -> IO ()
logOut ip = do
  conn <- open "tools.db"
  withExclusiveTransaction conn $ do
    execute conn "DELETE FROM logins WHERE ip = ?" (Only ip)
  close conn

logIn :: String -> String -> IO (Maybe String)
logIn username ip = do
  conn <- open "tools.db"
  (withExclusiveTransaction conn $ do
    userList <- query conn "SELECT * FROM users WHERE userName = ?" (Only username) :: IO [User]
    case (foundAny userList) of
      Nothing -> do
        return Nothing
      Just user -> do
        execute conn "INSERT INTO logins (keeperName, ip) VALUES (?, ?)" (userName user , ip)
        return $ Just "success") <* (close conn)

getAllWalletsByName :: Connection -> String -> IO (Maybe [Wallet])
getAllWalletsByName conn username = do
  user <- selectUser conn username
  case user of
    Nothing -> return Nothing
    Just x -> do
      foundAll <$> query conn "SELECT * FROM wallets WHERE keeperName = ?" (Only username) :: IO (Maybe [Wallet])

historyFromWallets :: Connection -> [Wallet] -> IO (Maybe [String])  
historyFromWallets conn listWallets = do
  Just <$> ((++ [";"]) . concat) <$> (sequence $ map (\wallet -> do
    hF <- query conn "SELECT * FROM history WHERE walletFrom = ? OR walletTo = ? ORDER BY sqltime DESC" (walletId wallet, walletId wallet) :: IO [History]    
    return $ ["<", (publicId wallet)] ++ (map (show) hF) ++ [">"]
    ) listWallets)

historyByTime :: Connection -> Wallet -> IO (Maybe String)
historyByTime conn wallet = do
  hF <- query conn "SELECT * FROM history WHERE walletFrom = ? OR walletTo = ? ORDER BY sqltime DESC" (walletId wallet, walletId wallet) :: IO [History]    
  let headListHistories = head hF
  let tailListHistories = tail hF
  let foldStrH listHistories = foldl (\f s -> f ++ "|" ++ (show s)) (show headListHistories) tailListHistories  
  return $ Just $ "<" ++ (foldStrH hF) ++ ">"

getFixedHistory :: String -> String -> IO (Maybe String)
getFixedHistory username walletId = do
  conn <- open "tools.db"
  maybeWallet <- selectWalletByPublicId conn walletId
  (case maybeWallet of
    Nothing -> return Nothing
    Just wallet ->
      historyByTime conn wallet) <* (close conn)

getUserHistory :: String -> IO (Maybe [String])
getUserHistory username = do
  conn <- open "tools.db"
  wallets <- getAllWalletsByName conn username
  (case wallets of
    Nothing -> return Nothing
    Just listWallets ->
      historyFromWallets conn listWallets) <* (close conn)

        

