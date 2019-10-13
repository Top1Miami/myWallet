--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time
import Data.Bits (xor, (.&.), shift)
import Data.Char (ord, chr)
import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import Data.Strings
import Data.List (intercalate, take, length)
import Data.Text hiding (head, tail, map, foldl, words, init, tail, any, reverse, intercalate, take, length)
import System.IO
import Graphics.UI.Gtk hiding (Action, backspace, Socket)
import System.Environment
import System.Directory
import System.Random (randomRIO)
import Control.Exception
import Control.Lens ((.~), (^?), element,(&))

getUpperButtons :: Window -> Window -> Window -> Handle -> Bool -> IO(HBox)
getUpperButtons startWindow prevWindow window hdl checkRem = do
  -- btnExit <- buttonNewWithLabel "Exit"
  -- btnExit `on` buttonActivated $ do
  --   widgetHideAll window
  --   widgetShowAll startWindow
  btnBack <- buttonNewWithLabel "Back"
  btnBack `on` buttonActivated $ do
    widgetHideAll window
    widgetShowAll prevWindow
    widgetDestroy window
  btnRem <- buttonNewWithLabel "Forget me"
  btnRem `on` buttonActivated $ do
    hPutStrLn hdl "logout"
    widgetHideAll window
    widgetShowAll startWindow
  when (not checkRem) $ do set btnRem [ widgetVisible := False ]
  hbox <- hBoxNew False 5
  -- boxPackStart hbox btnExit PackNatural 0
  boxPackStart hbox btnBack PackNatural 0
  boxPackStart hbox btnRem PackNatural 0
  return hbox

getFormatedWindow :: Handle -> IO ((Window, VBox))
getFormatedWindow hdl = do
  window <- windowNew
  window `on` deleteEvent $ do
    liftIO $ hClose hdl
    liftIO $ mainQuit
    return False
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 
             -- , windowResizable    := False
             -- , windowAllowGrow     := False
             ]
  windowSetDefaultSize window 300 300
  vbAll <- vBoxNew False 5 
  containerAdd window vbAll
  return (window, vbAll)

createTransferWindow :: Handle -> [Int] -> Window -> Window -> Window -> String -> String -> Bool -> IO (Window)
createTransferWindow hdl key startWindow prevWindow loginWindow username info checkRem = do
  (window, vbAll) <- getFormatedWindow hdl

  hbox <- getUpperButtons startWindow prevWindow window hdl checkRem
  boxPackStart vbAll hbox PackNatural 0
  labelName <- labelNewWithMnemonic "username"
  entName <- entryNew
  set entName [ entryText := "" ]
  labelAmount <- labelNewWithMnemonic "amount of money to transfer"
  entAmount <- entryNew
  set entAmount [ entryText := "" ]
  btnTransfer <- buttonNewWithLabel "transfer"
  boxPackStart vbAll labelName PackGrow 0
  boxPackStart vbAll entName PackGrow 0
  boxPackStart vbAll labelAmount PackGrow 0
  boxPackStart vbAll entAmount PackGrow 0
  boxPackStart vbAll btnTransfer PackGrow 0
  btnTransfer `on` buttonActivated $ do
    let (_, other) = strSplit "|" info
    entAmountStr <- entryGetText entAmount
    entNameStr <- entryGetText entName
    let checkForUnused = foldl (\x c -> x && (any (c == ) "abcdefghikjlmnopqrstuvwxyzABCDEFGHIKJLMNOPQRSTXYZ1234567890")) True entNameStr
    let checkNumber = foldl (\x c -> x && (any (c == ) "1234567890.")) True entAmountStr
    if(not checkNumber)
      then do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only numbers"  
        widgetShow dialogW
      else
        if(not checkForUnused)
          then do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only letters either case, numbers"  
            widgetShow dialogW
          else do
            let (walletType, searchId) = strSplit "-" other
            putStrLn $ "transfer " ++ username ++ " " ++ searchId ++ " " ++ entNameStr ++ " " ++ entAmountStr
            hPutStrLn hdl $ encrypt key $ "transfer " ++ username ++ " " ++ searchId ++ " " ++ entNameStr ++ " " ++ entAmountStr
            output <- (decrypt key) <$> hGetLine hdl
            when (output == "failure") $ do
              dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Something went wrong and opertaion failed. Try again later."  
              widgetShow dialogW
            putStrLn output
            widgetHideAll window
            widgetShowAll loginWindow
            widgetDestroy window
            widgetDestroy prevWindow
  return window

createTransferInsideWindow :: Handle -> [Int] -> Window -> Window -> Window -> String -> String -> Bool -> IO (Window)
createTransferInsideWindow hdl key startWindow prevWindow loginWindow username info checkRem = do
  (window, vbAll) <- getFormatedWindow hdl
  hbox <- getUpperButtons startWindow prevWindow window hdl checkRem
  boxPackStart vbAll hbox PackNatural 0
  wallType <- comboBoxNewText
  
  window `on` mapEvent $ do 
    liftIO $ hPutStrLn hdl $ encrypt key $ "info " ++ username
    info <- liftIO $ (decrypt key) <$> hGetLine hdl
    let typeList = map (\x -> fst $ strSplit "-" $ snd $ strSplit "|" x) $ words info
    liftIO $ sequence $ map (comboBoxAppendText wallType . pack) typeList
    return False

  entAmount <- entryNew
  set entAmount [ entryText := "" ]
  btnTransfer <- buttonNewWithLabel "transfer" 
  boxPackStart vbAll wallType PackGrow 0
  boxPackStart vbAll entAmount PackGrow 0
  boxPackStart vbAll btnTransfer PackGrow 0
  btnTransfer `on` buttonActivated $ do
    entAmountStr <- entryGetText entAmount
    let checkNumber = foldl (\x c -> x && (any (c == ) "1234567890.")) True entAmountStr
    if(not checkNumber)
      then do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only numbers"  
        widgetShow dialogW
      else do 
        selectedVal <- comboBoxGetActiveText wallType
        case selectedVal of
          Nothing -> do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please choose wallet type"  
            widgetShow dialogW
          Just wall -> do
            let (walletType, _) = strSplit "-" $ snd $ strSplit "|" info
            hPutStrLn hdl $ encrypt key $ "change " ++ username ++ " " ++ walletType ++ " " ++ (tail $ init (show wall)) ++ " " ++ entAmountStr
            output <- (decrypt key) <$> hGetLine hdl
            when (output == "failure") $ do
              dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Something went wrong and opertaion failed. Try again later."  
              widgetShow dialogW
            putStrLn output
            widgetHideAll window
            widgetShowAll loginWindow
            widgetDestroy window
            widgetDestroy prevWindow
  return window

createButtonWindow :: Handle -> [Int] -> Window -> Window -> String -> String -> Bool -> IO (Window)
createButtonWindow hdl key startWindow prevWindow username info checkRem = do
  (window, vbAll) <- getFormatedWindow hdl
  hbox <- getUpperButtons startWindow prevWindow window hdl checkRem
  boxPackStart vbAll hbox PackNatural 0
  notebookW <- notebookNew
  boxPackStart vbAll notebookW PackGrow 0
  vbIO <- vBoxNew False 0
  vbH <- vBoxNew False 0
  notebookAppendPage notebookW vbIO "info and opertions"
  notebookAppendPage notebookW vbH "history"
  label <- labelNewWithMnemonic ""
  boxPackStart vbH label PackGrow 0    
  window `on` mapEvent $ do 
    liftIO $ putStrLn "dropped"
    (x,y) <- liftIO $ windowGetDefaultSize window
    liftIO $ hPutStrLn hdl $ encrypt key $ "getHistory " ++ username ++ " " ++ (snd (strSplit "-" info))
    liftIO $ putStrLn "dropped"
    output <- liftIO $ (decrypt key) <$> hGetLine hdl
    liftIO $ putStrLn "dropped"
    liftIO $ set label [ labelLabel := (intercalate "\n" $ strSplitAll "|" $ tail $ init output) ]
    return False
  labelInfo <- labelNewWithMnemonic $ username ++ " has " ++ info
  btnTransfer <- buttonNewWithLabel "transfer to other person"
  btnTransfer `on` buttonActivated $ do
    transferWindow <- createTransferWindow hdl key startWindow window prevWindow username info checkRem
    widgetHideAll window
    widgetShowAll transferWindow
  btnChange <- buttonNewWithLabel "to your wallet"
  btnChange `on` buttonActivated $ do
    transferWindow <- createTransferInsideWindow hdl key startWindow window prevWindow username info checkRem
    widgetHideAll window
    widgetShowAll transferWindow
  boxPackStart vbIO labelInfo PackGrow 0
  boxPackStart vbIO btnTransfer PackGrow 0
  boxPackStart vbIO btnChange PackGrow 0
  return window

date :: IO (Integer,Int,Int)
date = getCurrentTime >>= return . toGregorian . utctDay

recuresivelyWrite :: Handle -> String -> IO ()
recuresivelyWrite hdl fileToWrite = do
  output <- hGetLine hdl
  when (output /= ";") $ do
    when (output /= "<" || output /= ">") $ do
      appendFile fileToWrite output
    recuresivelyWrite hdl fileToWrite 

createLoginWindow :: Handle -> [Int] -> Window -> String -> String -> Bool -> IO (Window)
createLoginWindow  hdl key startWindow username info checkRem = do
  (window, vbAll)<- getFormatedWindow hdl
  window `on` mapEvent $ do 
    (x,y) <- liftIO $ windowGetDefaultSize window
    liftIO $ putStrLn $ (show x) ++ " " ++ (show y)
    return False
  -- btnExit <- buttonNewWithLabel "Exit"
  -- btnExit `on` buttonActivated $ do
  --   widgetHideAll window
  --   widgetShowAll startWindow
  btnRem <- buttonNewWithLabel "Forget Me"
  btnRem `on` buttonActivated $ do
    hPutStrLn hdl "logout"
    widgetHideAll window
    widgetShowAll startWindow
  when (not checkRem) $ do set btnRem [ widgetVisible := False ]
  btnGetFullHistory <- buttonNewWithLabel "History"
  btnGetFullHistory `on` buttonActivated $ do
    hPutStrLn hdl $ encrypt key $ "getFullHistory " ++ username
    doesDE <- doesDirectoryExist "history"
    when (not doesDE) $ do createDirectory "history"
    (y, m, d) <- date
    let fileToWrite = "history/" ++ username ++ (show y) ++ (show m) ++ (show d) ++ ".txt"
    -- curTime <- getCurrentTime
    -- let fileToWrite = "history/" ++ username ++ (show curTime) ++ ".txt"
    writeFile fileToWrite ""
    recuresivelyWrite hdl fileToWrite

  hUpperBox <- hBoxNew False 5
  btnRefresh <- buttonNewWithLabel "Refresh"
  btnRefresh `on` buttonActivated $ do
    hPutStrLn hdl $ encrypt key $ "info " ++ username
    updatedInfo <- (decrypt key) <$> hGetLine hdl
    redrawnWindow <- createLoginWindow hdl key startWindow username updatedInfo checkRem
    widgetHideAll window
    widgetShowAll redrawnWindow
    widgetDestroy window
  -- boxPackStart hUpperBox btnExit PackNatural 0
  boxPackStart hUpperBox btnRefresh PackNatural 0
  boxPackStart hUpperBox btnRem PackNatural 0
  boxPackStart hUpperBox btnGetFullHistory PackNatural 0
  boxPackStart vbAll hUpperBox PackNatural 0
  
  vbWallets <- vBoxNew False 5
  vbAddWallet <- vBoxNew False 5
  notebookW <- notebookNew
  boxPackStart vbAll notebookW PackGrow 0
  wallType <- comboBoxNewText
  sequence $ map (comboBoxAppendText wallType . pack) ["dollar", "euro", "ruble", "bitcoin"]
  notebookAppendPage notebookW vbWallets "wallets"
  notebookAppendPage notebookW vbAddWallet "add new wallet"
  btnCreate <- buttonNewWithLabel "create"
  btnCreate `on` buttonActivated $ do
    selectedVal <- comboBoxGetActiveText wallType
    case selectedVal of
      Nothing -> do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please choose wallet type"  
        widgetShow dialogW
      Just x -> do
        putStrLn $ "combobox = " ++ show x
        hPutStrLn hdl $ encrypt key $ "createWallet " ++ username ++ " " ++ (tail $ init (show x))
        info <- (decrypt key) <$> hGetLine hdl
        when (info /= "failure") $ do
          putStrLn "added"
          btnToAdd <- createButtonFromWallet vbWallets hdl key startWindow window username checkRem ("0|" ++ (tail $ init (show x)))
          -- boxPackStart vbWallets btnToAdd PackGrow 0
          widgetShow btnToAdd
  when (info /= "NotFound") $ do void $ sequence $ map (createButtonFromWallet vbWallets hdl key startWindow window username checkRem) (words info)
  
  widgetShowAll vbWallets
  boxPackStart vbAddWallet wallType PackGrow 0
  boxPackStart vbAddWallet btnCreate PackGrow 0

  return window
  
  where
    createButtonFromWallet :: VBox -> Handle -> [Int] -> Window -> Window -> String -> Bool -> String -> IO (Button)
    createButtonFromWallet vbox hdl key startWindow prevWindow username checkRem info  = do
      btn <- buttonNewWithLabel info
      let (amount, wallType) = (strSplit "|" info)
      boxPackStart vbox btn PackGrow 0
      btn `on` buttonActivated $ do
        if ((read amount :: Double) /= 0) 
          then do 
            putStrLn $ "amount parse " ++ show amount
            window <- createButtonWindow hdl key startWindow prevWindow username info checkRem
            putStrLn "window created"
            widgetHideAll prevWindow
            widgetShowAll window
          else do
            dialogW <- messageDialogNew (Just startWindow) [DialogDestroyWithParent] MessageWarning ButtonsNone "There is no operation availabele with no resources on account"  
            widgetShow dialogW
      return btn

createRegisterWindow :: Handle -> [Int] -> Window -> IO (Window)
createRegisterWindow hdl key startWindow = do
  (window, vbAll) <- getFormatedWindow hdl
  btnBack <- buttonNewWithLabel "Back"
  btnBack `on` buttonActivated $ do
    widgetHideAll window
    widgetShowAll startWindow
  boxPackStart vbAll btnBack PackNatural 0
  hbName <- hBoxNew False 5
  labelName <- labelNewWithMnemonic "username"
  entName <- entryNew
  set entName [ entryText := "" ]
  boxPackStart hbName labelName PackGrow 5
  boxPackStart hbName entName PackGrow 5
  boxPackStart vbAll hbName PackGrow 0

  hbMail <- hBoxNew False 5
  labelMail <- labelNewWithMnemonic "mail"
  entMail <- entryNew
  set entMail [ entryText := "" ]
  boxPackStart hbMail labelMail PackGrow 5
  boxPackStart hbMail entMail PackGrow 5
  boxPackStart vbAll hbMail PackGrow 0

  hbPsw <- hBoxNew False 5
  labelPsw <- labelNewWithMnemonic "password"
  entPsw <- entryNew
  set entPsw [ entryText := "" ]
  boxPackStart hbPsw labelPsw PackGrow 5
  boxPackStart hbPsw entPsw PackGrow 5
  boxPackStart vbAll hbPsw PackGrow 0


  hbPswRepeat <- hBoxNew False 5
  labelPswRepeat <- labelNewWithMnemonic "repeat password"
  entPswRepeat <- entryNew
  set entPswRepeat [ entryText := "" ]
  boxPackStart hbPswRepeat labelPswRepeat PackGrow 5
  boxPackStart hbPswRepeat entPswRepeat PackGrow 5
  boxPackStart vbAll hbPswRepeat PackGrow 0

  confirm <- checkButtonNewWithLabel "Confirm Policy"
  btnRegister <- buttonNewWithLabel "Register"

  btnRegister `on` buttonActivated $ do
    array <- sequence $ map (entryGetText) [entName, entPsw, entMail]
    arrayTwo <- sequence $ map (entryGetText) [entName, entPsw, entMail, entPswRepeat]::IO([String])
    pswrd <- entryGetText entPsw :: IO (String)
    pswrdRepeat <- entryGetText entPswRepeat
    let (check, pswEqual) = (foldl (\x y -> x && not (strNull y)) True array, pswrd == pswrdRepeat)
    let checkForUnused = foldl (\xStr singleStr -> xStr && (foldl (\x c -> x && (any (c == ) "abcdefghikjlmnopqrstuvwxyzABCDEFGHIKJLMNOPQRSTXYZ1234567890@")) True singleStr)) True arrayTwo
    confirmed <- toggleButtonGetActive confirm
    if(not checkForUnused)
      then do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only letters either case, numbers"  
        widgetShow dialogW
      else 
        if(confirmed && check && pswEqual)
          then do
            hPutStrLn hdl $ encrypt key $ "register " ++ (foldl (\x y -> x ++ " " ++ y) "" array)
            widgetShowAll startWindow
            widgetHideAll window
          else do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please fill all the specified fieilds and accept policies"  
            widgetShow dialogW

  boxPackStart vbAll confirm PackGrow 0
  boxPackStart vbAll btnRegister PackGrow 0

  return window 

createStartWindow :: Handle -> [Int] -> IO (Window)
createStartWindow hdl key = do 
  (window, vbAll) <- getFormatedWindow hdl
  window `on` mapEvent $ do 
    liftIO $ hPutStrLn hdl $ encrypt key "autologin"
    liftIO $ putStrLn "startedWork"
    result <- liftIO $ (decrypt key) <$> hGetLine hdl
    liftIO $ putStrLn result
    liftIO $ when(result /= "failure") $ do
      -- nameAndInfo <- hGetLine hdl
      let (info, name) = strSplit " " result
      putStrLn $ "name = " ++ name ++ " info = " ++ info
      nextWindow <- createLoginWindow hdl key window name info True
      widgetHideAll window
      widgetShowAll nextWindow
    return True

  image <- imageNewFromFile "logo.png"
  boxPackStart vbAll image PackGrow 5

  hbLogin <- hBoxNew False 5
  boxPackStart vbAll hbLogin PackGrow 5

  vbLogin <- vBoxNew True 2
  entName <- entryNew
  set entName [ entryText := "input username" ]
  entPsw <- entryNew
  set entPsw [ entryText := "input password" ]
  entName `on` entryActivated $ do
    entrySetText entName ""
  entPsw `on` entryActivated $ do
    entrySetText entPsw ""
    entrySetVisibility entPsw False
  rem <- checkButtonNewWithLabel "Remember me"
  boxPackStart vbLogin entName PackGrow 5
  boxPackStart vbLogin entPsw PackGrow 5
  boxPackStart hbLogin vbLogin PackGrow 40
  boxPackStart hbLogin rem PackGrow 2

  btnLogin <- buttonNewWithLabel "Login"
  -- set btnLogin [ widgetWidthRequest := 80]
  btnLogin `on` buttonActivated $ do
    array <- sequence $ map (entryGetText) [entName, entPsw]
    hPutStrLn hdl $ encrypt key $ "login " ++ (foldl (\x y -> x ++ " " ++ y) "" array)
    info <- (decrypt key) <$> hGetLine hdl 
    when (info /= "failure") $ do
      checkRem <- toggleButtonGetActive rem
      when (checkRem == True) $ do
        username <- entryGetText entName
        hPutStrLn hdl $ encrypt key $ "savelogin " ++ username
      loginWindow <- createLoginWindow hdl key window (head array) info checkRem
      widgetHideAll window
      widgetShowAll loginWindow

  boxPackStart vbAll btnLogin PackNatural 5
  btnRegister <- buttonNewWithLabel "Register"
  -- set btnRegister [ widgetWidthRequest := 80]
  btnRegister `on` buttonActivated $ do
    widgetHideAll window
    registerWindow <- createRegisterWindow hdl key window
    widgetShowAll registerWindow
  
  boxPackStart vbAll btnRegister PackNatural 5
  return $ window

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

bitPow :: Integer -> Integer -> Integer-> Integer
bitPow _ 0 _ = 1
bitPow n power byMod = if(power .&. 1 == 1)
  then
    ((n *) $! (bitPow n (power - 1) byMod)) `mod` byMod
  else
    let b = bitPow n (shift power $ -1) byMod in
      (b * b) `mod` byMod

runDiffieHellman :: Handle -> Int -> IO ([Int])
runDiffieHellman hdl k = do
  putStrLn "in runDiffieHellman"
  a <- randomRIO (92233720368547758, 9223372036854775807) :: IO (Integer)
  putStrLn $ show a
  let aSend = fromIntegral (bitPow 3681993451487 a 8429605667295912267) :: Int -- convert
  let aInteger = (bitPow 3681993451487 a 8429605667295912267)
  putStrLn $ "tosend not converted = " ++ (show aInteger) 
  putStrLn $ show aSend
  hPutStrLn hdl $ show aSend
  b <- hGetLine hdl
  putStrLn $ "aSend = " ++ (show aSend) ++ " b Receieved = " ++ b 
  let key = fromIntegral (bitPow (read b :: Integer) a 8429605667295912267) :: Int  -- convert
  let keyInteger = (bitPow (read b :: Integer) a 8429605667295912267)
  putStrLn $ "from other = " ++ b
  putStrLn $ "key = " ++ (show key)
  putStrLn $ "key not converted = " ++ (show key)
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

main :: IO ()
main = do
  arguments <- getArgs
  let port = head arguments
  let servAddr = head $ tail arguments
  let hints = defaultHints {
                addrSocketType = Stream
              , addrFamily = AF_INET
              }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ head arguments)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  withFdSocket sock $ setCloseOnExecIfNeeded
  connect sock $ addrAddress addr

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  putStrLn "Started runDiffieHellman protocol "
  key <- runDiffieHellman hdl 7
  putStrLn "Key is :"
  putStrLn $ show key
  
  void initGUI

  window <- createStartWindow hdl key
  widgetShowAll window

  mainGUI


 