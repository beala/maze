{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foundation

import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Control.Monad.State.Strict as St
import qualified Control.Monad as M
import Data.Foldable (foldMap)
import Options.Generic

import qualified System.Random as R

--import qualified Debug.Trace as Debug

newtype Loc = Loc (Int, Int) deriving (Eq, Ord, Show)
type Edge = (Loc, Loc)

instance Monoid Loc where
  mempty = Loc (0,0)
  mappend (Loc (x1,y1)) (Loc (x2, y2)) = Loc (x1+x2, y1+y2)

type Random a = St.State R.StdGen a

data Options = Options
  { solveOpt :: Bool <?> "Solve the maze."
  , seedOpt :: Maybe Int <?> "Seed to generate the maze."
  , widthOpt :: Int <?> "Width of the maze."
  , heightOpt :: Int <?> "Height of the maze."
  } deriving (Generic, Show)

instance ParseRecord Options

-- | Classic depth first search maze generator.
maze :: Loc -> -- Current location
        (Int, Int) -> -- Boundary (upper right corner)
        S.Set Loc -> -- Visited nodes
        Random (T.Tree Loc, S.Set Loc)
maze loc boundary visited = do
  shuffledNeighbors <- shuffle $ adjacent loc -- Shuffle your neighbors
  (children, newVisited) <- M.foldM visit ([], visited) shuffledNeighbors -- Visit them
  return (T.Node loc children, newVisited)
    where visit (children, visitedInner) aNeighbor =
            if isValid visitedInner aNeighbor boundary then do
              let newV = S.insert aNeighbor visitedInner
              (t, v) <- maze aNeighbor boundary newV
              return (t : children, v)
            else return (children, visitedInner)

-- | Generate a list of adjacent locations
adjacent :: Loc -> [Loc]
adjacent loc = do
  d <- [Loc (1,0), Loc (-1,0), Loc (0,1), Loc (0,-1)]
  return (d <> loc)

-- | Check that a node is valid (inside the boundary of the maze)
-- | and hasn't been visited yet.
isValid :: S.Set Loc -> Loc -> (Int, Int) -> Bool
isValid visited loc@(Loc (x,y)) (maxHeight, maxWidth) =
  (not (S.member loc visited)) &&
  x <= maxHeight && y <= maxWidth &&
  x >= 0 && y >= 0

newtype Solution = Solution {unSolution :: [Loc]} deriving (Eq, Show)

-- | Monoid that takes the nonempty solution. If both are nonempty, then combine them.
-- | Use this to short circuit to a solution in `solve`
instance Monoid Solution where
  mempty = Solution []
  mappend (Solution x) (Solution y) = if null x then Solution y else Solution x

-- | Solve with DFS, returning a list of the nodes in the path.
solve :: Loc -> T.Tree Loc -> [Loc]
solve end (T.Node loc children) =
  if loc == end then [loc]
  else unSolution $ foldMap visit children
  where visit child =
          let path = solve end child in
            if null path then mempty
            else Solution (loc : path)

neighbors :: S.Set Edge -> Loc -> [Loc]
neighbors edges loc =
  let relevantEdges = S.filter isNeighbor edges
      uniqNeighbors = foldMap (\(_,e) -> S.singleton e) relevantEdges
  in
    S.toList uniqNeighbors
  where isNeighbor (n1, _) = loc == n1

-- | Bespoke shuffle. Not sure why this doesn't exist already ><
-- | Shuffle the tail, insert the head randomly.
shuffle :: [a] -> Random [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle (x:xs) = do
  xs' <- shuffle xs
  let maxI = (fromCount (length xs))
  i <- St.state $ R.randomR (0, maxI)
  let (h,t) = splitAt (CountOf i) xs'
  return $ h <> (x : t)

-- | This is kind of cool in a horrible way. Use the list monad
-- | to loop over the maze from left to right and pretty print it.
mazeToString :: S.Set Edge -> (Int, Int) -> S.Set Loc -> [Char]
mazeToString edges (width, height) solution =
  let mazeStr = do
        y <- [height, height-1 .. 0]
        x <- [0..width]
        (dir,open,closed) <- [(Loc (0, -1), ' ', '_'), (Loc (1, 0), '_', '|')]
        let nl = if x == width && dir == Loc (1,0) then "\n" else ""
        let wall = if x == 0 && dir == Loc (0,-1) then " |" else ""
        let c =  if (S.member (Loc (x,y), Loc (x,y) <> dir)  edges) || (S.member (Loc (x,y) <> dir, Loc (x,y)) edges) then [open] else [closed]
        let cc =  if S.member (Loc (x,y)) solution then "\27[31m"<> c <>"\27[0m" else c 
        wall <> cc <> nl
      top = " " <> replicate (CountOf (width*2)+1) '_' <> "\n"
  in
    top <> mazeStr

allEdges :: T.Tree Loc -> S.Set Edge
allEdges (T.Node loc children) =
  let childEdges = foldMap (\(T.Node l _) -> S.singleton (loc, l)) children
      otherEdges = foldMap allEdges children
  in
    childEdges <> otherEdges

main :: IO ()
main = do
  options :: Options <- getRecord "Maze generator."
  let width = unHelpful $ widthOpt options
  let height = unHelpful $ heightOpt options
  let seed = unHelpful $ seedOpt options
  let shouldSolve = unHelpful $ solveOpt options
  let boundary = (width,height)

  -- Generate the maze
  let start = Loc (0,0)
  let m = maze start boundary (S.singleton start) 
  rand <- case seed of
    Just s -> return $ R.mkStdGen s
    Nothing -> R.getStdGen
  let (treeMaze, _) = St.evalState m rand

  -- Solve
  let solution = solve (Loc boundary) treeMaze

  -- Print it out.
  let edges = allEdges treeMaze
  if shouldSolve then
    putStrLn $ fromList $ mazeToString edges boundary (S.fromList solution)
  else
    putStrLn $ fromList $ mazeToString edges boundary (S.empty)
