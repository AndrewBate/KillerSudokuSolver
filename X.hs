
module X (Grid(..),solve,make_grid,select_row) where
import qualified Data.IntMap as M
import Data.IntMap ((!),IntMap)
import qualified Data.IntSet as S
import Data.IntSet (IntSet)
import Data.List (minimumBy)
import Data.Function (on)
import Control.Monad.State

type Rows = IntMap IntSet
type Cols = IntMap IntSet

-- Grid - a set of intersections where each intersection represents a 1 in a
-- sparse binary matrix
type Grid = (Rows,Cols)


-- Algorithm x to solve exact cover problem
-- (select a set of rows such that each column has exactly one 1 in it)
-- empty cols = success
-- empty rows = FAIL

solve :: Grid -> [Int] -> [[Int]]
solve (rows,cols) solAcc
    | M.null cols                = return solAcc
    | any S.null  $ M.elems cols = fail ""
    | otherwise                  = do
          let col = minimumBy (compare `on` S.size) $ M.elems cols -- the selected colum
          row <- S.elems col                -- the selected row
          let newgrid = select_row (rows,cols) row
          solve newgrid (row:solAcc)


select_row :: Grid ->  -- | the grid we select the row of
             Int -> -- | the row index to select
             Grid
select_row grid@(rows,cols) row =
    let rem_cols = S.elems $ rows !row -- the colums that this row eliminates
                                       -- (colums this row sets)
        -- the rows that these columns elminate (other rows that would also set
        -- this column)
        rem_rows = concatMap (\col -> S.elems $ cols ! col) rem_cols  in
    execState (do
        mapM_ (modify . remove_row) rem_rows
        mapM_ (modify . remove_col) rem_cols) grid


remove_row, remove_col :: Int -> Grid -> Grid
remove_row rn (rows,cols) =
    let rows' = M.delete rn rows
        cols' = fmap (S.delete rn) cols
    in (rows',cols')

remove_col cn (rows,cols) =
    let cols' = M.delete cn cols
        rows' = fmap (S.delete cn) rows
    in (rows',cols')

make_grid :: [(Int,Int)] -> Grid
make_grid positions =
    add_to_grid (M.empty,M.empty) positions

add_to_grid :: Grid -> [(Int,Int)] -> Grid
add_to_grid grid [] = grid
add_to_grid (rows,cols) ((r,c):rcs) =
    add_to_grid (add rows r c, add cols c r) rcs

add :: IntMap IntSet -> Int -> Int -> IntMap IntSet
add m i sv =
    case M.lookup i m of
      Nothing -> M.insert i (S.singleton sv) m
      Just s  -> M.insert i (S.insert sv s)  m

