module Main where
import Network.Socket
import Control.Concurrent
import System.IO
import System.Environment
import Data.Strings
import Control.Monad.Fix (fix)
import Lib (
    addWallet
  , addUser
  , transferMoney
  , verifyUser
  , transferWallet
  , depositWallet
  , autoLogin
  , logOut
  , logIn
  )

data SqlAction = SaveLogin String String | Logout String | AutoLogin String | Login String String | Register String String String | Info String | Transfer String String String String | Change String String String String | CrWallet String String String | Deposit String String String deriving (Show)

toSqlAction :: [String] -> SqlAction
toSqlAction list =
  -- Rewrite
  let comm = head list in case comm of
    "login" -> Login (list !! 1) (list !! 2)
    "register" -> Register (list !! 1) (list !! 2) (list !! 3)
    "createWallet" -> CrWallet (list !! 1) (list !! 2) "0"
    "transfer" -> Transfer (list !! 1) (list !! 2) (list !! 3) (list !! 4)
    "change" -> Change (list !! 1) (list !! 2) (list !! 3) (list !! 4) 
    "deposit" -> Deposit (list !! 1) (list !! 2) (list !! 3)
    "info" -> Info $ list !! 1
    "autologin" -> AutoLogin $ list !! 1
    "logout" -> Logout $ list !! 1
    "savelogin" -> SaveLogin (list !! 1) (list !! 2)
 
evalActionInfo :: Handle -> Maybe String -> IO (Bool)
evalActionInfo hdl Nothing = do
  putStrLn "failure"
  hPutStrLn hdl "failure"
  return $ False
evalActionInfo hdl (Just output) = do
  putStrLn output
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
performAction hdl (Transfer from to walletType toTransfer) = do
  transferred <- transferMoney from to walletType (read toTransfer)
  evalActionInfo hdl transferred
performAction hdl (Change username from to toTransfer) = do
  transferred <- transferWallet username from to (read toTransfer)
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
  -- evalActionInfo hdl logedOut


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
    let inputSplit = words (input ++ " " ++ ip)
    res <- performAction hdl $ toSqlAction inputSplit
    putStrLn $ show res
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
