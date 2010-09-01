module Killer ( KillerBox (..),
                solve_killer) where
import X(make_grid ,solve ,Grid)
import Data.List (delete,sort)
import Control.Monad (guard)

sum_perms :: (Num t, Num a, Ord a) => a -> t -> [a] -> [[a]]
sum_perms 0 0 _ = return []
sum_perms rem_sum n l = do
  sel <- l
  guard $ rem_sum - sel >= 0
  sels <- sum_perms (rem_sum - sel) (n-1) (delete sel l)
  return (sel:sels)

data KillerBox = KillerBox { total :: Int
                           , cells :: [(Int,Int)] }

make_x_rows :: KillerBox -> [([((Int,Int),Int)], -- The cell numbering in the XRow
                            [Int] ) ] -- The colum numbers in this row
make_x_rows box =
    let perms = sum_perms (total box) (length $ cells box) [1..9]
        matched = map (zip $ cells box) perms
        x_rows = map (concatMap (\((rows',cols'),nums') ->
                                    let rows = rows' - 1
                                        cols = cols' - 1
                                        nums = nums' - 1 in
                                     -- number in every cell
                                    [      9 * cols + rows
                                    -- each number in every column
                                    ,81  + 9 * cols + nums
                                    -- each number in every row
                                    ,162 + 9 * rows + nums
                                    -- each number in every box
                                    ,243 + 9 * (3 * (cols `div` 3) + rows `div` 3) + nums ] )) matched

    in
      zip matched x_rows

make_killer :: [KillerBox] -> (Grid,[[((Int,Int),Int)]])
make_killer boxes =
    let (cell_contents,xrows) = unzip $ concatMap make_x_rows boxes
        x_coords = concatMap (\(rn,cols) -> zip (repeat rn) cols) $ zip [0..] xrows in
    (make_grid x_coords, cell_contents)

solve_killer :: [KillerBox] -> [[((Int,Int),Int)]]
solve_killer boxes =
    let (xgrid,xrow_meanings) = make_killer boxes
        sol = (map sort)  $ solve xgrid [] in
    map (sort . (concatMap (xrow_meanings !!))) sol
