

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
  
  
  killer_boxes <- newIORef ([] :: [KillerBox])
            
  set window [ windowTitle := "Sudoku Solver"
             , containerBorderWidth := 20
             ]

  sudoku_grid <- tableNew 9 9 True
  toggle_buttons<- mapM (\(r,c) -> do
                         button <- toggleButtonNew
                         tableAttachDefaults sudoku_grid button (r-1) r (c-1) c
                         return ((r,c),button) ) $ range ((1,1) ,(9,9))
  
  number_setting <- vBoxNew False 10
  number_entry <- entryNew

  number_accept <- buttonNewWithLabel "add box"
  onClicked number_accept $ do
         n <- fmap read (entryGetText number_entry)
         (cells,activeButtons) <- bstate toggle_buttons
         modifyIORef killer_boxes ((KillerBox n cells):)
         box_number <- fmap length $ readIORef killer_boxes
         mapM_ (\tb -> do 
                  toggleButtonSetActive tb False
                  buttonSetLabel tb $ show (n,box_number) ) activeButtons
                         

  solve_button <- buttonNewWithLabel "solve"
  onClicked solve_button $ do
         boxes <- readIORef killer_boxes
         let (soln:_) = solve_killer boxes
         zipWithM_ (\(p1,n) (p2,button) -> buttonSetLabel button (show n)) soln toggle_buttons 


  append number_setting number_entry
  append number_setting number_accept
  append number_setting solve_button

  hbox <- hBoxNew False 20
  append hbox sudoku_grid
  append hbox number_setting
  
  containerAdd window hbox
  widgetShowAll window

  mainGUI
  

append box thing = boxPackStart box thing PackGrow 0

bstate buttons = fmap unzip (filterM (toggleButtonGetActive . snd) buttons)
                  

    
