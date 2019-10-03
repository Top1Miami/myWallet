module Main where
import Network.Socket
import Data.Bits (xor)
import Data.Char (ord, chr)
import Control.Concurrent
import System.IO
import System.Environment
import System.Random (randomRIO)
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

data IndexExceptionOnInitialization = IndexExceptionOnInitialization Int Int deriving (Show)

instance Exception IndexExceptionOnInitialization where

initializeBlock :: [Int] -> [Int]
initializeBlock key = shufle 0 0 start key
  where
    start = [0..255]
    shufle :: Int -> Int -> [Int] -> [Int] -> [Int]
    shufle _ 256 list _ = list 
    shufle base i list key = shufle j (i + 1) (subList & element j .~ swap) key 
      where
        j = mod (base + (list -!!- i) + (key -!!- (mod i $ length key))) 256 
        swap = list -!!- i
        subList = (list & element i .~ (list -!!- j))

encrypt :: [Int] -> String -> String
encrypt key msg = Prelude.map chr $ xorEval 0 0 (initializeBlock key) msg
  where
    xorEval :: Int -> Int -> [Int] -> String -> [Int]
    xorEval _ _ _ [] = [] 
    xorEval i j sBlock (m:msg) = (xor (ord m :: Int) (finBlock -!!- t)) : (xorEval newI newJ finBlock msg)
      where
        newI = mod (i + 1) 256
        newJ = mod (j + (sBlock -!!- i)) 256 
        swap = sBlock -!!- i
        subBlock = sBlock & element i .~ (sBlock -!!- j)
        finBlock = subBlock & element j .~ swap
        t = mod ((finBlock -!!- newI) + (finBlock -!!- newJ)) 256

decrypt :: [Int] -> String -> String
decrypt = encrypt

(-!!-) :: [a] -> Int -> a
(-!!-) list number = case list ^? element number of
  Nothing -> throw (IndexExceptionOnInitialization number (length list))
  Just x -> x

runDiffieHellman :: Handle -> Int -> IO ([Int])
runDiffieHellman hdl k = do
  putStrLn "in runDiffieHellman"
  a <- randomRIO (92233720368547758, 9223372036854775807) :: IO (Int)
  putStrLn $ show a
  let aSend = mod ((3681993451487) ^ a) 8429605667295912267 :: Int
  putStrLn $ show aSend
  hPutStrLn hdl $ show aSend
  b <- hGetLine hdl
  putStrLn $ "aSend = " ++ (show aSend) ++ " b Receieved = " ++ b 
  let key = mod ((read b :: Int) ^ a) 8429605667295912267 :: Int
  putStrLn $ "key = " ++ (show key)
  let roundOne = take k $ separateToBit key
  putStrLn $ show roundOne
  if(length roundOne < k)
    then do
      keyAdd <- runDiffieHellman hdl (k - (length roundOne))
      return $ roundOne ++ keyAdd
    else
      return roundOne
  where
    separateToBit :: Int -> [Int]
    separateToBit 0 = []
    separateToBit num = (mod num 256) : (separateToBit (num `div` (256::Int)))

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sockAddr) = do
  -- putStrLn $ show sockAddr
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  let (ip, port) = strSplit ":" $ show sockAddr

  putStrLn "Started runDiffieHellman protocol "
  key <- runDiffieHellman hdl 7
  putStrLn "Key is :"
  putStrLn $ show key
  fix $ \loop -> do
    putStrLn "called"
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