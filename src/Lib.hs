{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( addWallet
  , addUser
  , transferMoney
  , verifyUser
  , transferWallet
  , depositWallet
  , autoLogin
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

data Login = Login { loginId :: Int
                   , loginKeeperId :: Int
                   , ip :: String
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

instance FromRow Login where
  fromRow = Login <$> field <*> field <*> field  

addUser :: String -> String -> String -> IO (Maybe String)
addUser username password mail = do
  conn <- open "tools.db"
  withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> do
        execute conn "INSERT INTO users (username, password, mail) VALUES (?, ?, ?)" (username, password, mail)
        close conn
        return $ (Just "success")
      Just user -> do
        close conn
        return Nothing

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
      then fmap Just $ (getAllWallets user) >>= (maybe (return "NotFound") (\list -> return $ foldl (\conc arg -> conc ++ (show arg) ++ " ") "" list))
      else return $ Nothing



addWallet :: String -> String -> Int -> IO (Maybe String)
addWallet username walletType amountM = do
  conn <- open "tools.db"  
  res <- withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of 
      Nothing -> do 
        putStrLn "user not found, operation failed"
        -- close conn
        return Nothing
      Just user -> do
          maybeWallet <- selectWallet conn user walletType
          case maybeWallet of
            Nothing -> do 
              execute conn "INSERT INTO wallets (keeperId, walletType, amountM) VALUES (?, ?, ?)" (userId user, walletType, amountM)
              -- close conn
              return $ Just $ "success"
            Just wallet -> do
              -- close conn
              return Nothing
  close conn
  return res

selectWallet :: Connection -> User -> String -> IO (Maybe Wallet)
selectWallet conn user walletType = do
  response <- query conn "SELECT * FROM wallets WHERE keeperId = ? AND walletType = ?" (userId user, walletType)
  return $ foundAny response

selectWalletById :: Connection -> Int -> IO (Maybe Wallet)
selectWalletById conn walletId = do
  response <- query conn "SELECT * FROM wallets WHERE id = ?" (Only walletId)
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
  res <- withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    eitherMinus <- updateMinus conn toTransfer from maybeUser
    eitherPlus <- updatePlus conn to maybeUser
    let eitherBoth = liftA2 (\x y -> (x, y, toTransfer)) eitherMinus eitherPlus
      
    either (\x -> return $ Nothing) (onSuccess conn) eitherBoth
  close conn
  return res


updateMinus :: Connection -> Int -> String -> Maybe User -> IO (Either Int Wallet)
updateMinus _ _ _ Nothing = return $ Left 0
updateMinus conn amount walletType (Just user) = do
  maybeWallet <- selectWallet conn user walletType
  case maybeWallet of 
    Nothing -> return $ Left $ -2
    Just wallet -> if(amountM wallet >= amount)
      then         
        return $ Right wallet
      else do
        putStrLn "transaction failed"
        return $ Left $ -1

updatePlus :: Connection -> String -> Maybe User -> IO (Either Int Wallet)
updatePlus _ _ Nothing = return $ Left 0
updatePlus conn walletType (Just user) = do
  maybeWallet <- selectWallet conn user walletType
  case maybeWallet of 
    Nothing -> return $ Left $ -1
    Just wallet -> do
      return $ Right wallet


transferMoney :: String -> String -> String -> Int -> IO (Maybe String)
transferMoney from to walletType toTransfer = do
  conn <- open "tools.db"
  res <- withExclusiveTransaction conn $ do
    maybeFrom <- selectUser conn from
    maybeTo <- selectUser conn to
    putStrLn "here"
    eitherMinus <- updateMinus conn toTransfer walletType maybeFrom
    eitherPlus <- updatePlus conn walletType maybeTo
    let eitherBoth = liftA2 (\x y -> (x, y, toTransfer)) eitherMinus eitherPlus

    either (\x -> return $ Nothing) (onSuccess conn) eitherBoth
  close conn
  return res 


onSuccess :: Connection -> (Wallet, Wallet, Int) -> IO (Maybe String)
onSuccess conn (walletFrom, walletTo, money) = do  
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
  withExclusiveTransaction conn $ execute conn "DELETE FROM users WHERE username = ?" (Only username)
  close conn

deleteWallet :: String -> String -> IO ()
deleteWallet username walletType = do
  conn <- open "tools.db"
  withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> putStrLn "user not found, operation failed"
      Just user -> do 
        execute conn "DELETE FROM users WHERE keeperId = ? AND walletType = ?" (userId user, walletType)
  close conn

depositWallet :: String -> String -> String -> IO (Bool)
depositWallet username walletType amount = do
  conn <- open "tools.db"
  res <- withExclusiveTransaction conn $ do
    maybeUser <- selectUser conn username
    case maybeUser of
      Nothing -> do 
        putStrLn "user not found, operation failed"
        return False
      Just user -> do
        execute conn "UPDATE wallets SET amountM = ? WHERE keeperId = ? AND walletType = ?" ((read amount):: Int, userId user, walletType)
        return True
  close conn
  return res

autoLogin :: String -> IO (Maybe String)
autoLogin ip = do
  conn <- open "tools.db"
  login <- query conn "SELECT * FROM logins WHERE ip = ?" (Only ip) :: IO [Login]
  case (foundAny login) of
    Nothing -> do 
      close conn
      return $ Just "unloged"
    Just x -> do
      userList <- query conn "SELECT * FROM users WHERE id = ?" (Only $ loginKeeperId x) :: IO [User]
      close conn
      (Just <$> (maybe (return Nothing) getAllWallets (foundAny userList)) >>= (maybe (return "NotFound") (\list -> return $ foldl (\conc arg -> conc ++ (show arg) ++ " ") "" list))
      