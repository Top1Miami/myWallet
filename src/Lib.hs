{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( addWallet
  , addUser
  , transferMoney
  , verifyUser
  , transferWallet
  ) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import Data.Maybe

data User = User { userId :: Int
                 , userName :: String
                 , password :: String
                 , mail :: String
                 }
data Wallet = Wallet { walletId :: Int
                     , keeperId :: Int
                     , walletType :: String
                     , amountM :: Int 
                     }

instance Show User where
  show user = mconcat [ show $ userId user
                      , ") username : "
                      , userName user
                      , ", password : "
                      , password user
                      , "\nmail : " 
                      , mail user
                      ]
instance Show Wallet where
  show wallet = mconcat [ show $ amountM wallet
                        , "|"
                        , walletType wallet
                        ] 

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance FromRow Wallet where
  fromRow = Wallet <$> field <*> field <*> field <*> field

-- @TODO write on stms, clean imports and write nice functions to delete extra code 

dbAction :: String -> (Connection -> IO ()) -> IO ()
dbAction dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> String -> String -> IO (Maybe String)
addUser username password mail = do
  conn <- open "tools.db"
  maybeUser <- selectUser conn username
  case maybeUser of
    Nothing -> do
      execute conn "INSERT INTO users (username, password, mail) VALUES (?, ?, ?)" (username, password, mail)
      close conn
      return $ (Just "success")
    Just user -> do
      close conn
      return $ Nothing

foundAny :: [a] -> Maybe a
foundAny [] = Nothing
foundAny (x:_) = Just x 

selectUser :: Connection -> String -> IO (Maybe User)
selectUser conn username = do
  response <- query conn "SELECT * FROM users WHERE username = (?)" (Only username)
  return $ foundAny response

verifyUser :: String -> String -> IO (Maybe String)
verifyUser username pwd = do
  conn <- open "tools.db"
  maybeUser <- selectUser conn username
  putStrLn $ show maybeUser
  case maybeUser of
    Nothing -> return Nothing
    Just user -> if ((password user) == pwd)
      then fmap Just $ (getAllWallets user) >>= (maybe (return "NotFound") (\list -> return $ foldl (\conc arg -> conc ++ (show arg) ++ " ") "" list))
      else return $ Nothing

addWallet :: String -> String -> Int -> IO (Maybe String)
addWallet username walletType amountM = do
  conn <- open "tools.db"  
  maybeUser <- selectUser conn username
  case maybeUser of 
    Nothing -> do 
      putStrLn "user not found, operation failed"
      close conn
      return Nothing
    Just user -> do
        maybeWallet <- selectWallet conn user walletType
        case maybeWallet of
          Nothing -> do 
            execute conn "INSERT INTO wallets (keeperId, walletType, amountM) VALUES (?, ?, ?)" (userId user, walletType, amountM)
            close conn
            return $ Just $ "success"
          Just wallet -> do
            close conn
            return Nothing

selectWallet :: Connection -> User -> String -> IO (Maybe Wallet)
selectWallet conn user walletType = do
  response <- query conn "SELECT * FROM wallets WHERE keeperId = ? AND walletType = ?" (userId user, walletType)
  return $ foundAny response

selectWalletById :: Connection -> Int -> IO (Maybe Wallet)
selectWalletById conn walletId = do
  response <- query conn "SELECT * FROM wallets WHERE walletId = ?" (Only walletId)
  return $ foundAny response

foundAll :: [a] -> Maybe [a]
foundAll [] = Nothing
foundAll list = Just list 

getAllWallets :: User -> IO (Maybe [Wallet])
getAllWallets user = do
  conn <- open "tools.db"
  response <- query conn "SELECT * FROM wallets WHERE keeperId = ?" (Only $ userId user)
  close conn
  return $ foundAll $ response

transferWallet :: String -> String -> String -> Int -> IO (Maybe String)
transferWallet username from to toTransfer = do
  conn <- open "tools.db"
  maybeUser <- selectUser conn username
  close conn
  eitherMinus <- updateMinus toTransfer from maybeUser
  eitherPlus <- updatePlus to maybeUser
  let eitherBoth = liftA2 (\x y -> (x, y, toTransfer)) eitherMinus eitherPlus
    
  either (\x -> return $ Nothing) onSuccess eitherBoth

updateMinus :: Int -> String -> Maybe User -> IO (Either Int Wallet)
updateMinus _ walletType Nothing = return $ Left 0
updateMinus amount walletType (Just user) = do
  conn <- open "tools.db"
  maybeWallet <- selectWallet conn user walletType
  close conn
  case maybeWallet of 
    Nothing -> return $ Left $ -2
    Just wallet -> if(amountM wallet >= amount)
      then         
        return $ Right wallet
      else do
        putStrLn "transaction failed"
        return $ Left $ -1

updatePlus :: String -> Maybe User -> IO (Either Int Wallet)
updatePlus walletType Nothing = return $ Left 0
updatePlus walletType (Just user) = do
  conn <- open "tools.db"
  maybeWallet <- selectWallet conn user walletType
  close conn
  case maybeWallet of 
    Nothing -> return $ Left $ -1
    Just wallet -> do
      return $ Right wallet


transferMoney :: String -> String -> String -> Int -> IO (Maybe String)
transferMoney from to walletType toTransfer = do
  conn <- open "tools.db"
  maybeFrom <- selectUser conn from
  maybeTo <- selectUser conn to
  close conn
  eitherMinus <- updateMinus toTransfer walletType maybeFrom
  eitherPlus <- updatePlus walletType maybeTo
  let eitherBoth = liftA2 (\x y -> (x, y, toTransfer)) eitherMinus eitherPlus

  either (\x -> return $ Nothing) onSuccess eitherBoth
  


onSuccess :: (Wallet, Wallet, Int) -> IO (Maybe String)
onSuccess (walletFrom, walletTo, money) = do  
  dbAction "tools.db" $
    \conn -> do
      execute conn "UPDATE wallets SET amountM = ? WHERE id = ?" (amountM walletFrom - money, walletId walletFrom)
      execute conn "UPDATE wallets SET amountM = ? WHERE id = ?" (amountM walletTo + money, walletId walletTo)
  conn <- open "tools.db"
  maybeFrom <- selectWalletById conn (walletId walletFrom)
  maybeTo <- selectWalletById conn (walletId walletTo)
  close conn
  return $ getWalletInfo maybeFrom maybeTo
  where
    getWalletInfo :: Maybe Wallet -> Maybe Wallet -> Maybe String
    getWalletInfo Nothing _ = Nothing
    getWalletInfo _ Nothing = Nothing
    getWalletInfo (Just from) (Just to) = Just (show from ++ show to)

deleteUser :: String -> IO ()
deleteUser username = 
  dbAction "tools.db" $
    \conn -> do
      execute conn "DELETE FROM users WHERE username = ?" (Only username)

deleteWallet :: String -> String -> IO ()
deleteWallet username walletType = 
  dbAction "tools.db" $
    \conn -> do
      maybeUser <- selectUser conn username
      case maybeUser of
        Nothing -> putStrLn "user not found, operation failed"
        Just user -> do 
          execute conn "DELETE FROM users WHERE keeperId = ? AND walletType = ?" (userId user, walletType)
