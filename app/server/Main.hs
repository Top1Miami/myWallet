module Main where
import Network.Socket
import Control.Concurrent
import System.IO
import System.Environment
import Data.Strings
import Control.Lens
import Control.Exception
import Control.Monad.Fix (fix)
import Lib (
    addWallet
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
  )

data SqlAction = GetFullHistory String | GetHistory String String | SaveLogin String String | Logout String | AutoLogin String | Login String String | Change String String String String | Register String String String | Info String | Transfer String String String String | CrWallet String String String | Deposit String String String deriving (Show)

data ActionConvertionException = ActionConvertionException deriving (Show)

instance Exception ActionConvertionException where

toSqlAction :: [String] -> String -> SqlAction
toSqlAction list ip =
  let comm = head list in case comm of
    "login" -> Login (list -!!- 1) (list -!!- 2)
    "register" -> Register (list -!!- 1) (list -!!- 2) (list -!!- 3)
    "createWallet" -> CrWallet (list -!!- 1) (list -!!- 2) "0"
    "transfer" -> Transfer (list -!!- 1) (list -!!- 2) (list -!!- 3) (list -!!- 4)
    "deposit" -> Deposit (list -!!- 1) (list -!!- 2) (list -!!- 3)
    "info" -> Info $ list -!!- 1
    "autologin" -> AutoLogin $ ip
    "logout" -> Logout ip
    "savelogin" -> SaveLogin (list -!!- 1) ip
    "change" -> Change (list -!!- 1) (list -!!- 2) (list -!!- 3) (list -!!- 4)
    "getHistory" -> GetHistory (list -!!- 1) (list -!!- 2)
    "getFullHistory" -> GetFullHistory (list -!!- 1)
    _ -> throw ActionConvertionException
  where 
    (-!!-) :: [a] -> Int -> a
    (-!!-) list number = case list ^? element number of
      Nothing -> throw ActionConvertionException
      Just x -> x

evalActionInfo :: Handle -> Maybe String -> IO (Bool)
evalActionInfo hdl Nothing = do
  putStrLn "failure"
  hPutStrLn hdl "failure"
  return $ False
evalActionInfo hdl (Just output) = do
  putStrLn $ "toSend = " ++ output
  hPutStrLn hdl output
  return $ True

performAction :: Handle -> SqlAction -> IO (Bool)
performAction hdl login@(Login username password) = do
  putStrLn $ show login
  verified <- verifyUser username password False
  evalActionInfo hdl verified
performAction hdl (Register username password mail) = do
  added <- addUser username password mail
  return $ maybe False (\x -> True) added
performAction hdl (Transfer username from to toTransfer) = do
  transferred <- transferMoneyById from to (read toTransfer)
  evalActionInfo hdl transferred
performAction hdl (CrWallet username walletType amount) = do
  added <- addWallet username walletType (read amount) 
  evalActionInfo hdl added
performAction hdl (Deposit username walletType amount) = do
  depositWallet username walletType (read amount)
performAction hdl (Info username) = do
  verified <- verifyUser username "trash" True
  evalActionInfo hdl verified
performAction hdl (AutoLogin ip) = do
  logedIn <- autoLogin ip
  evalActionInfo hdl logedIn
performAction hdl (Logout ip) = do
  logOut ip
  return True
performAction hdl (SaveLogin username ip) = do
  logIn username ip
  return True
performAction hdl (Change username from to amount) = do
  changed <- transferMoneyBetween username from to (read amount)
  evalActionInfo hdl changed
performAction hdl (GetHistory username walletId) = do
  info <- getFixedHistory username walletId
  evalActionInfo hdl info
performAction hdl (GetFullHistory username) = do
  info <- getUserHistory username
  checkIfSend hdl info
  where
    checkIfSend :: Handle -> Maybe [String] -> IO (Bool)
    checkIfSend hdl Nothing = do
      putStrLn "failure"
      return False
    checkIfSend hdl (Just list) = do
      sendAll hdl list
      return True

    sendAll :: Handle -> [String] -> IO () 
    sendAll hdl (x:rest) = do
      hPutStrLn hdl x
      sendAll hdl rest
    sendAll hdl [] = return ()
      
mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sockAddr) = do
  -- putStrLn $ show sockAddr
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  let (ip, port) = strSplit ":" $ show sockAddr

  fix $ \loop -> do
    input <- hGetLine hdl
    putStrLn input
    let inputSplit = words input
    res <- (try $ performAction hdl $ toSqlAction inputSplit ip) :: IO (Either ActionConvertionException Bool)
    case res of
      Left ex -> putStrLn $ show ex
      Right x -> putStrLn $ show x
    loop

main :: IO ()
main = do
  arguments <- getArgs
  let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              , addrFamily = AF_INET
              }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just $ head arguments)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock $ setCloseOnExecIfNeeded
  bind sock (addrAddress addr)
  listen sock 10
  mainLoop sock
