module Main (main) where

import Control.Monad
-- import Control.Concurrent (threadDelay)

import Paths

import Logic

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    _ <- return w # set title "Tic Tac Toe"
    UI.addStyleSheet w "buttons.css"

    button1 <- mkButton' "O" "1"
    button2 <- mkButton' "O" "2"
    button3 <- mkButton' "O" "3"
    button4 <- mkButton' "O" "4"
    button5 <- mkButton' "O" "5"
    button6 <- mkButton' "O" "6"
    button7 <- mkButton' "O" "7"
    button8 <- mkButton' "O" "8"
    button9 <- mkButton' "O" "9"
    getBody w #+
        [UI.div #. "wrap" #+ (
          greet ++ [
            grid [
              [(element button1), (element button2), (element button3)],
              [(element button4), (element button5), (element button6)],
              [(element button7), (element button8), (element button9)]
            ]
          ]
        )]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Tic Tac Toi"]
    ]

mkButton' :: String -> String -> UI Element
mkButton' t id_ = do
     button <- UI.button #. ("button" ++ id_) #+ [string t]
     on UI.click button $ \_ -> do
       liftIO $ actionOnClick id_
     -- view   <- UI.p #+ [element button]
     return button


actionOnClick :: String -> IO ()
actionOnClick str = putStrLn $ "Cliked: " ++ str

{-
mkButtons' :: UI [Element]
mkButtons' = do
  b1 <- mkButton' "X" "1"
  on UI.click b1 $ \_ -> do
    element b1 # set text "y"
  b2 <- mkButton' "O" "1"
  b3 <- mkButton' "X" "1"

  return [b1, b2, b3]
mkButton :: String -> UI (Element, Element)
mkButton title = do
    button <- UI.button #. "button" #+ [string title]
    view   <- UI.p #+ [element button]
    return (button, view)

mkButtons :: UI [Element]
mkButtons = do
    list    <- UI.ul #. "buttons-list"

    (button1, view1) <- mkButton button1Title

    on UI.hover button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [hover]")
    on UI.leave button1 $ \_ -> do
        element button1 # set text button1Title
    on UI.click button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [pressed]")
        liftIO $ threadDelay $ 1000 * 1000 * 1
        element list    #+ [UI.li # set html "<b>Delayed</b> result!"]

    (button2, view2) <- mkButton button2Title

    on UI.hover button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [hover]")
    on UI.leave button2 $ \_ -> do
        element button2 # set text button2Title
    on UI.click button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [pressed]")
        element list    #+ [UI.li # set html "Zap! Quick result!"]

    return [list, view1, view2]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately" -}

--viewSource :: UI Element
--viewSource = UI.p #+
--    [UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]]
--    where
--    url = samplesURL ++ "Buttons.hs"
--    url = samplesURL ++ "Buttons.hs"

