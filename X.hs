
module X where
import qualified Data.IntMap as M
import Data.IntMap ((!),IntMap)
import qualified Data.IntSet as S
import Data.IntSet (IntSet)
import Data.List
import Data.Function (on)

type Rows = IntMap IntSet
type Cols = IntMap IntSet

type Grid = (Rows,Cols)


-- Algorithm x
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
select_row (rows,cols) row =
  let remCols = S.elems $ rows !row -- the colums that this row eliminates
      -- the rows that this colum elminates
      remRows = concatMap (\col -> S.elems $ cols ! col) remCols  in
  remove_rows_and_cols (rows,cols) remRows remCols

remove_row, remove_col :: Grid -> Int -> Grid
remove_row (rows,cols) rn =
    let rows' = M.delete rn rows
        cols' = fmap (S.delete rn) cols 
    in (rows',cols')

remove_col (rows,cols) cn =
    let cols' = M.delete cn cols
        rows' = fmap (S.delete cn) rows
    in (rows',cols')

remove_rows_and_cols :: Grid -> [Int] -> [Int] -> Grid
remove_rows_and_cols grid rows cols =
    let rows_removed = foldl remove_row grid rows
        cols_removed = foldl remove_col rows_removed cols in
    cols_removed


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

