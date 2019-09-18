--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import Data.Strings
import Data.Text hiding (head, tail, map, foldl, words, init, tail, any, reverse)
import System.IO
import Graphics.UI.Gtk hiding (Action, backspace, Socket)
import System.Environment
import System.Directory
-- availableWallets :: [String]
-- availableWallets = ["dollar", "euro", "ruble", "bitcoin"]
getUpperButtons :: Window -> Window -> Window -> IO(HBox)
getUpperButtons startWindow prevWindow window = do
  btnExit <- buttonNewWithLabel "Exit"
  btnExit `on` buttonActivated $ do
    checkExist <- doesFileExist "login.txt"
    liftIO $ when (checkExist) $ do
      removeFile "login.txt"
    widgetHideAll window
    widgetShowAll startWindow
  btnBack <- buttonNewWithLabel "Back"
  btnBack `on` buttonActivated $ do
    widgetHideAll window
    widgetShowAll prevWindow
  hbox <- hBoxNew False 5
  boxPackStart hbox btnExit PackNatural 0
  boxPackStart hbox btnBack PackNatural 0
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
             , windowResizable    := False
             -- , windowAllowGrow     := False
             ]
  windowSetDefaultSize window 300 300
  vbAll <- vBoxNew False 5 
  containerAdd window vbAll
  return (window, vbAll)

createTransferWindow :: Handle -> Window -> Window -> String -> String -> IO (Window)
createTransferWindow hdl startWindow prevWindow username info = do
  (window, vbAll) <- getFormatedWindow hdl

  hbox <- getUpperButtons startWindow prevWindow window
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
    let (amount, wallType) = (strSplit "|" info)
    entAmountStr <- entryGetText entAmount
    entNameStr <- entryGetText entName
    let checkForUnused = foldl (\x c -> x && (any (c == ) "abcdefghikjlmnopqrstuvwxyzABCDEFGHIKJLMNOPQRSTXYZ1234567890")) True entNameStr
    let checkNumber = foldl (\x c -> x && (any (c == ) "1234567890")) True entAmountStr
    if(not checkNumber)
      then do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only numbers"  
        widgetShow dialogW
      else
        if(not checkForUnused)
          then do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only letters either case, numbers"  
            widgetShow dialogW
          else 
            if((read amount::Int) > (read entAmountStr::Int))
              then do
                putStrLn $! "transfer " ++ username ++ " " ++ entNameStr ++ " " ++ wallType ++ " " ++ entAmountStr
                hPutStrLn hdl $! "transfer " ++ username ++ " " ++ entNameStr ++ " " ++ wallType ++ " " ++ entAmountStr
                output <- hGetLine hdl
                putStrLn output
                widgetHideAll window
                widgetShowAll prevWindow
              else do
                dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Not enough money on account"  
                widgetShow dialogW
   
  return window

createTransferInsideWindow :: Handle -> Window -> Window -> String -> String -> IO (Window)
createTransferInsideWindow hdl startWindow prevWindow username info = do
  (window, vbAll) <- getFormatedWindow hdl

  hbox <- getUpperButtons startWindow prevWindow window
  boxPackStart vbAll hbox PackNatural 0
  wallType <- comboBoxNewText
  sequence $ map (comboBoxAppendText wallType . pack) ["dollar", "euro", "ruble", "bitcoin"]
  entAmount <- entryNew
  set entAmount [ entryText := "" ]
  btnTransfer <- buttonNewWithLabel "transfer" 
  boxPackStart vbAll wallType PackGrow 0
  boxPackStart vbAll entAmount PackGrow 0
  boxPackStart vbAll btnTransfer PackGrow 0
  btnTransfer `on` buttonActivated $ do
    selectedVal <- comboBoxGetActiveText wallType
    case selectedVal of
      Nothing -> do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please choose wallet type"  
        widgetShow dialogW
      Just wall -> do
        let (amount, wallType) = (strSplit "|" info)
        entAmountStr <- entryGetText entAmount
        if((read amount::Int) > (read entAmountStr::Int))
          then do
            hPutStrLn hdl $ "change " ++ username ++ " " ++ wallType ++ " " ++ (show wall) ++ " " ++ entAmountStr
            output <- hGetLine hdl
            putStrLn output
            widgetHideAll window
            widgetShowAll prevWindow
          else do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Not enough money on account"  
            widgetShow dialogW
  return window



createButtonWindow :: Handle -> Window -> Window -> String -> String -> IO (Window)
createButtonWindow hdl startWindow prevWindow username info = do
  (window, vbAll) <- getFormatedWindow hdl
  hbox <- getUpperButtons startWindow prevWindow window
  boxPackStart vbAll hbox PackNatural 0
  labelInfo <- labelNewWithMnemonic $ username ++ " has " ++ info
  btnTransfer <- buttonNewWithLabel "transfer to other person"
  btnTransfer `on` buttonActivated $ do
    transferWindow <- createTransferWindow hdl startWindow window username info
    widgetHideAll window
    widgetShowAll transferWindow
  btnChange <- buttonNewWithLabel "to your wallet"
  boxPackStart vbAll labelInfo PackGrow 0
  boxPackStart vbAll btnTransfer PackGrow 0
  boxPackStart vbAll btnChange PackGrow 0
  return window


createLoginWindow :: Handle -> Window -> String -> String ->IO (Window)
createLoginWindow  hdl startWindow username info = do
  (window, vbAll)<- getFormatedWindow hdl
  window `on` mapEvent $ do 

    (x,y) <- liftIO $ windowGetDefaultSize window
    liftIO $ putStrLn $ (show x) ++ " " ++ (show y)
    return False
  btnExit <- buttonNewWithLabel "Exit"
  btnExit `on` buttonActivated $ do
    checkExist <- doesFileExist "login.txt"
    liftIO $ when (checkExist) $ do
      removeFile "login.txt"
    widgetHideAll window
    widgetShowAll startWindow
  hUpperBox <- hBoxNew False 5
  btnRefresh <- buttonNewWithLabel "Refresh"
  btnRefresh `on` buttonActivated $ do
    hPutStrLn hdl $ "info " ++ username
    updatedInfo <- hGetLine hdl
    redrawnWindow <- createLoginWindow hdl startWindow username updatedInfo
    widgetHideAll window
    widgetShowAll redrawnWindow
    widgetDestroy window
  boxPackStart hUpperBox btnExit PackNatural 0
  boxPackStart hUpperBox btnRefresh PackNatural 0
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
        hPutStrLn hdl $ "createWallet " ++ username ++ " " ++ (tail $ init (show x))
        info <- hGetLine hdl
        when (info == "success") $ do
          putStrLn "added"
          btnToAdd <- createButtonFromWallet vbWallets hdl startWindow window username ("0|" ++ (tail $ init (show x)))
          boxPackStart vbWallets btnToAdd PackGrow 0
          widgetShow btnToAdd
  res <- sequence $ map (createButtonFromWallet vbWallets hdl startWindow window username) (words info)
  
  widgetShowAll vbWallets
  boxPackStart vbAddWallet wallType PackGrow 0
  boxPackStart vbAddWallet btnCreate PackGrow 0


  return window
  
  where
    createButtonFromWallet :: VBox -> Handle -> Window -> Window -> String -> String ->  IO (Button)
    createButtonFromWallet vbox hdl startWindow prevWindow username info = do
      btn <- buttonNewWithLabel info
      let (amount, wallType) = (strSplit "|" info)
      boxPackStart vbox btn PackGrow 0
      btn `on` buttonActivated $ do
        if ((read amount) /= 0) 
          then do  
            window <- createButtonWindow hdl startWindow prevWindow username info
            widgetHideAll prevWindow
            widgetShowAll window
          else do
            dialogW <- messageDialogNew (Just startWindow) [DialogDestroyWithParent] MessageWarning ButtonsNone "There is no operation availabele with no resources on account"  
            widgetShow dialogW
      return btn



createRegisterWindow :: Handle -> Window -> IO (Window)
createRegisterWindow hdl startWindow = do
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
    let checkForUnused = foldl (\xStr singleStr -> xStr && (foldl (\x c -> x && (any (c == ) "abcdefghikjlmnopqrstuvwxyzABCDEFGHIKJLMNOPQRSTXYZ1234567890")) True singleStr)) True arrayTwo
    confirmed <- toggleButtonGetActive confirm
    if(not checkForUnused)
      then do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Use only letters either case, numbers"  
        widgetShow dialogW
      else 
        if(confirmed && check && pswEqual)
          then do
            hPutStrLn hdl $! "register " ++ (foldl (\x y -> x ++ " " ++ y) "" array)
            widgetShowAll startWindow
            widgetHideAll window
          else do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please fill all the specified fieilds and accept policies"  
            widgetShow dialogW

  boxPackStart vbAll confirm PackGrow 0
  boxPackStart vbAll btnRegister PackGrow 0

  return window 


createStartWindow :: Handle -> IO (Window)
createStartWindow hdl = do 
  (window, vbAll) <- getFormatedWindow hdl
  window `on` mapEvent $ do 
    liftIO $ hPutStrLn hdl "autoLogin"
    result <- liftIO $ hGetLine hdl
    liftIO $ when(result /= "failure") $ do
      nextWindow <- createLoginWindow hdl window info  
    return False
      

  -- image <- imageNewFromFile "logo.png"
  -- boxPackStart vbAll image PackGrow 5

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
  set btnLogin [ widgetWidthRequest := 40]
  btnLogin `on` buttonActivated $ do
    array <- sequence $ map (entryGetText) [entName, entPsw]
    hPutStrLn hdl $ "login " ++ (foldl (\x y -> x ++ " " ++ y) "" array)
    info <- hGetLine hdl 
    when (info /= "failure") $ do
      checkRem <- toggleButtonGetActive rem
      when (checkRem) $ do
        writeFile "login.txt" (show array)
      loginWindow <- createLoginWindow hdl window (head array) info
      widgetHideAll window
      widgetShowAll loginWindow

  boxPackStart vbAll btnLogin PackNatural 5
  btnRegister <- buttonNewWithLabel "Register"
  btnRegister `on` buttonActivated $ do
    widgetHideAll window
    registerWindow <- createRegisterWindow hdl window
    widgetShowAll registerWindow
  
  boxPackStart vbAll btnRegister PackGrow 5
  return $ window


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
  
  void initGUI

  window <- createStartWindow hdl
  widgetShowAll window

  mainGUI


 