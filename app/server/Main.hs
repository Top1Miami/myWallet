module Main where
import Network.Socket
import Control.Concurrent
import System.IO
import System.Environment
import Control.Monad.Fix (fix)
import Lib (
    addWallet
  , addUser
  , transferMoney
  , verifyUser
  , transferWallet
  )

data SqlAction = Login String String | Register String String String | Transfer String String String String | Change String String String String | CrWallet String String String deriving (Show)

toSqlAction :: [String] -> SqlAction
toSqlAction list =
  let comm = head list in case comm of
    "login" -> Login (list !! 1) (list !! 2)
    "register" -> Register (list !! 1) (list !! 2) (list !! 3)
    "createWallet" -> CrWallet (list !! 1) (list !! 2) "0"
    "transfer" -> Transfer (list !! 1) (list !! 2) (list !! 3) (list !! 4)
    "change" -> Change (list !! 1) (list !! 2) (list !! 3) (list !! 4) 

evalActionInfo :: Handle -> Maybe String -> IO (Bool)
evalActionInfo hdl Nothing = do
  hPutStrLn hdl "failure"
  return $ False
evalActionInfo hdl (Just output) = do
  -- putStrLn output
  hPutStrLn hdl output
  return $ True

performAction :: Handle -> SqlAction -> IO (Bool)
performAction hdl login@(Login username password) = do
  putStrLn $ show login
  verified <- verifyUser username password
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

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  fix $ \loop -> do
    input <- hGetLine hdl
    putStrLn input
    let inputSplit = words input
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
