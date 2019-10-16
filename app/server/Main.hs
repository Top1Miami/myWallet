 module Main where
import Network.Socket
import Data.Word (Word64, Word8)
import Data.Bits (xor, (.&.), shift)
import Data.Char (ord, chr)
import Data.ByteString (ByteString, map, pack, unpack) 
import Data.ByteString.UTF8 (fromString, toString)
import Control.Concurrent
import System.IO
import System.Environment
import System.Random (randomRIO)
import Data.Strings hiding (fromString, toString)
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
    "autologin" -> AutoLogin ip
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

evalActionInfo :: Handle -> [Int] -> Maybe String -> IO (Bool)
evalActionInfo hdl key Nothing = do
  putStrLn "failure"
  -- hPutStrLnWrap hdl $ encryptWrap key "failure"
  hPutStrLnWrap hdl $ "failure"
  return $ False
evalActionInfo hdl key (Just output) = do
  putStrLn $ "toSend = " ++ output
  -- hPutStrLnWrap hdl $ encryptWrap key output
  hPutStrLnWrap hdl output
  return $ True

performAction :: Handle -> [Int] ->  SqlAction -> IO (Bool)
performAction hdl key login@(Login username password) = do
  putStrLn $ show login
  verified <- verifyUser username password False
  evalActionInfo hdl key verified 
  -- evalActionInfo hdl key verified
performAction hdl key (Register username password mail) = do
  added <- addUser username password mail
  return $ maybe False (\x -> True) added -- leave as it is?
performAction hdl key (Transfer username from to toTransfer) = do
  transferred <- transferMoneyById from to (read toTransfer)
  evalActionInfo hdl key transferred
performAction hdl key (CrWallet username walletType amount) = do
  added <- addWallet username walletType (read amount) 
  evalActionInfo hdl key added
performAction hdl key (Deposit username walletType amount) = do
  depositWallet username walletType amount
performAction hdl key (Info username) = do
  verified <- verifyUser username "trash" True
  evalActionInfo hdl key verified 
  -- evalActionInfo hdl key verified
performAction hdl key (AutoLogin ip) = do
  logedIn <- autoLogin ip
  evalActionInfo hdl key logedIn
performAction hdl key (Logout ip) = do
  logOut ip
  return True
performAction hdl key (SaveLogin username ip) = do
  logIn username ip
  return True
performAction hdl key (Change username from to amount) = do
  changed <- transferMoneyBetween username from to (read amount)
  evalActionInfo hdl key changed
performAction hdl key (GetHistory username walletId) = do
  info <- getFixedHistory username walletId
  evalActionInfo hdl key info
performAction hdl key (GetFullHistory username) = do
  info <- getUserHistory username
  evalActionInfo hdl key info

-- checkIfSend :: Handle -> [Int] -> Maybe String -> IO (Bool)
-- checkIfSend hdl _ Nothing = do
--   putStrLn "failure"
--   hPutStrLn "failure"
--   return False
-- checkIfSend hdl key (Just list) = do
--   sendAll hdl key list
--   return True

-- sendAll :: Handle -> [Int] -> String -> IO () 
-- sendAll hdl key (x:rest) = do
--   hPutStrLn hdl $ encrypt key x
--   sendAll hdl key rest
-- sendAll _ _ [] = return ()
      
mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock




runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sockAddr) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  let (ip, port) = strSplit ":" $ show sockAddr

  -- key <- runDiffieHellman hdl 7
  -- putStrLn "Key is :"
  -- putStrLn $ show key
  fix $ \loop -> do
    putStrLn "called"
    -- input <- decryptWrap key <$> tillEnd hdl
    input <- tillEnd hdl
    putStrLn input
    let inputSplit = words input
    res <- (try $ return $ toSqlAction inputSplit ip) :: IO (Either ActionConvertionException SqlAction)
    case res of
      Left ex -> putStrLn $ show ex
      Right x -> do 
        performAction hdl [] x
        putStrLn $ show x
    loop

hPutStrLnWrap :: Handle -> String -> IO ()
hPutStrLnWrap hdl inp = do
  hPutStrLn hdl $ inp ++ ":"

tillEnd :: Handle -> IO String
tillEnd hdl = do
  input <- hGetLine hdl
  -- check <- hReady hdl
  if(customEOF input)
    then
      return $ init input
    else do
      addInp <- tillEnd hdl
      return $ init input ++ addInp
  where
    customEOF inp = case (last inp) of
      ':' -> True
      otherwise -> False

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

