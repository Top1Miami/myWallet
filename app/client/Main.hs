--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import Data.Strings
import Data.Text hiding (head, tail, map, foldl, words, init, tail)
import System.IO
import Graphics.UI.Gtk hiding (Action, backspace, Socket)
import System.Environment

-- availableWallets :: [String]
-- availableWallets = ["dollar", "euro", "ruble", "bitcoin"]

getFormatedWindow :: Window -> IO ((Window, VBox))
getFormatedWindow startWindow = do
  window <- windowNew
  window `on` deleteEvent $ do
    liftIO $ widgetShowAll startWindow
    return False
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 ]
  vbAll <- vBoxNew False 5 
  containerAdd window vbAll
  return (window, vbAll)

createTransferWindow :: Socket -> Handle -> Window -> String -> String -> IO (Window)
createTransferWindow sock hdl startWindow username info =
  (window, vbAll) <- getFormatedWindow startWindow
  entName <- entryNew
  set entName [ entryText := "" ]
  entAmount <- entryNew
  set entAmount [ entryText := "" ]
  btnTransfer <- buttonNewWithLabel "transfer"
  btnTransfer `on` buttonActivated $ do
    array <- sequence $ map (entryGetText) [entName, entAmount] 
    let (amount, wallType) = (strSplit info)
    let entAmountStr = entryGetText entAmount
    if((read amount) > (read entAmountStr))
      then do
        hPutStrLn hdl $! "transfer" ++ username ++ (entryGetText entName) ++ info ++ entAmountStr
        widgetHideAll window
        widgetShowAll startWindow
      else do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Not enough money on account"  
        widgetShow dialogW 

createTransferInsideWindow :: Socket -> Handle -> Window -> String -> String -> IO (Window)
createTransferInsideWindow sock hdl startWindow username info = do
  (window, vbAll) <- getFormatedWindow startWindow
  
          
  


createButtonWindow :: Socket -> Handle -> Window -> String -> String -> IO (Window)
createButtonWindow sock hdl startWindow username info = do
  window <- windowNew
  window `on` deleteEvent $ do
    liftIO $ widgetShowAll startWindow
    return False
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 ]
  vbAll <- vBoxNew False 5 
  containerAdd window vbAll
  labelInfo <- labelNewWithMnemonic $ username ++ " has " ++ info
  btnTransmit <- buttonNewWithLabel "transfer to other person"
  btnTransmit `on` buttonActivated $ do
    transferWindow <- createTransferWindow sock hdl window username info
    widgetHideAll window
    widgetShowAll transferWindow
  btnChange <- buttonNewWithLabel "to your wallet"
  boxPackStart vbAll labelInfo PackGrow 0
  boxPackStart vbAll btnTransmit PackGrow 0
  boxPackStart vbAll btnChange PackGrow 0
  return window


createLoginWindow :: Socket -> Handle -> Window -> String -> String ->IO (Window)
createLoginWindow  sock hdl startWindow username info = do
  window <- windowNew
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 ]
  window `on` deleteEvent $ do
    liftIO $ widgetShowAll startWindow
    return False 

-- @TODO create notebook for changing pages to create wallets
  vbWallets <- vBoxNew False 5
  vbAddWallet <- vBoxNew False 5
  notebookW <- notebookNew
  containerAdd window notebookW
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
          btnToAdd <- createButtonFromWallet vbWallets sock hdl window username ("0|" ++ (show x))
          boxPackStart vbWallets btnToAdd PackGrow 0
          widgetShow btnToAdd
  res <- sequence $ map (createButtonFromWallet vbWallets sock hdl window username) (words info)
  
  widgetShowAll vbWallets
  boxPackStart vbAddWallet wallType PackGrow 0
  boxPackStart vbAddWallet btnCreate PackGrow 0


  return window
  
  where
    createButtonFromWallet :: VBox -> Socket -> Handle -> Window -> String -> String ->  IO (Button)
    createButtonFromWallet vbox sock hdl startWindow username info = do
      btn <- buttonNewWithLabel info
      let (amount, wallType) = strSplit info
      putStrLn $ "buttonCreated " ++ info
      boxPackStart vbox btn PackGrow 0
      btn `on` buttonActivated $ do
        if (amount /= 0) 
          then do  
            widgetHideAll startWindow
            window <- createButtonWindow sock hdl startWindow username info
            widgetShowAll window
          else do
            dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "There is no operation availabele with no resources on account"  
            widgetShow dialogW



      return btn



createRegisterWindow :: Socket -> Handle -> Window -> IO (Window)
createRegisterWindow sock hdl startWindow = do
  window <- windowNew
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 ]
  window `on` deleteEvent $ do
    liftIO $ widgetShowAll startWindow
    return False
  vbAll <- vBoxNew False 5 
  containerAdd window vbAll
  
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
    let check = foldl (\x y -> x && not (strNull y)) True array
    confirmed <- toggleButtonGetActive confirm
    if(confirmed && check)
      then do
        hPutStrLn hdl $! "register " ++ (foldl (\x y -> x ++ " " ++ y) "" array)
        liftIO $ widgetShowAll startWindow
        liftIO $ widgetHideAll window
      else do
        dialogW <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageWarning ButtonsNone "Please fill all the specified fieilds and accept policies"  
        widgetShow dialogW

  boxPackStart vbAll confirm PackGrow 0
  boxPackStart vbAll btnRegister PackGrow 0

  return window


createStartWindow :: Socket -> Handle -> IO (Window)
createStartWindow sock hdl = do 
  window <- windowNew
  set window [ windowTitle         := "WebDollas"
             , windowDefaultWidth  := 300
             , windowDefaultHeight := 300 ]
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  vbAll <- vBoxNew False 5
  containerAdd window vbAll

  image <- imageNewFromFile "logo.png"
  boxPackStart vbAll image PackGrow 5

  hbLogin <- hBoxNew False 5
  boxPackStart vbAll hbLogin PackGrow 5

  vbLogin <- vBoxNew True 2
  entName <- entryNew
  set entName [ entryText := "input username" ]
  entPsw <- entryNew
  set entPsw [ entryText := "input password" ]
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
    -- putStrLn info
    when (info /= "failure") $ do
      loginWindow <- createLoginWindow sock hdl window (head array) info
      liftIO $ widgetHideAll window
      liftIO $ widgetShowAll loginWindow

  boxPackStart vbAll btnLogin PackNatural 5
  btnRegister <- buttonNewWithLabel "Register"
  btnRegister `on` buttonActivated $ do
    widgetHideAll window
    registerWindow <- createRegisterWindow sock hdl window
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

  window <- createStartWindow sock hdl
  widgetShowAll window

  mainGUI


 