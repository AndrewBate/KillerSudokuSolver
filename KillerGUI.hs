

import Graphics.UI.Gtk
import Data.Array
import Control.Concurrent
import Control.Monad
import Data.IORef
import Killer


main :: IO ()
main = do 
  initGUI
  window <- windowNew
  
  
  killerBoxes <- newIORef ([] :: [KillerBox])
            
  set window [ windowTitle := "Sudoku Solver"
             , containerBorderWidth := 20
             ]

  sudokuGrid <- tableNew 9 9 True
  toggleButtons<- mapM (\(r,c) -> do
                         button <- toggleButtonNew
                         tableAttachDefaults sudokuGrid button (r-1) r (c-1) c
                         return ((r,c),button) ) $ range ((1,1) ,(9,9))
  
  numberSetting <- vBoxNew False 10
  numberEntry <- entryNew

  numberAccept <- buttonNewWithLabel "add box"
  onClicked numberAccept $ do
         n <- fmap read (entryGetText numberEntry)
         (cells,activeButtons) <- bstate toggleButtons
         modifyIORef killerBoxes ((KillerBox n cells):)
         soFar <- fmap length $ readIORef killerBoxes
         mapM_ (\tb -> do 
                  toggleButtonSetActive tb False
                  buttonSetLabel tb $ show (n,soFar) ) activeButtons
                         

  solveButton <- buttonNewWithLabel "solve"
  onClicked solveButton $ do
         boxes <- readIORef killerBoxes
         let (soln:_) = solveKiller boxes
         zipWithM_ (\(p1,n) (p2,button) -> buttonSetLabel button (show n)) soln toggleButtons 


  append numberSetting numberEntry
  append numberSetting numberAccept
  append numberSetting solveButton

  hbox <- hBoxNew False 20
  append hbox sudokuGrid
  append hbox numberSetting
  
  containerAdd window hbox
  widgetShowAll window

  mainGUI
  

append box thing = boxPackStart box thing PackGrow 0

bstate buttons = fmap unzip (filterM (toggleButtonGetActive . snd) buttons)
                  

    
