{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foundation

import qualified Data.Set as S
import qualified Control.Monad.State.Lazy as St
import qualified Control.Monad as M

import qualified System.Random as R
import Prelude (read)

--import qualified Debug.Trace as Debug

newtype Loc = Loc (Int, Int) deriving (Eq, Ord, Show)
type Edge = (Loc, Loc)

instance Monoid Loc where
  mempty = Loc (0,0)
  mappend (Loc (x1,y1)) (Loc (x2, y2)) = Loc (x1+x2, y1+y2)

type Random a = St.State R.StdGen a

-- | Classic depth first search maze generator.
maze :: Loc -> -- Current location
        (Int, Int) -> -- Boundary (upper right corner)
        S.Set Edge -> -- Edges
        S.Set Loc -> -- Visited nodes
        Random (S.Set Edge, S.Set Loc)
maze loc boundary edges visited = do
  neighbors <- shuffle $ adjacent loc -- Shuffle your neighbors
  M.foldM visit (edges, visited) neighbors -- Visit them in a random order
    where visit (edgesInner, visitedInner) aNeighbor =
            if isValid visitedInner aNeighbor boundary then do
              let newE = S.insert (loc, aNeighbor) edgesInner -- Your current node + next node forms an edge in the maze.
              let newV = S.insert aNeighbor visitedInner
              maze aNeighbor boundary newE newV
            else return (edgesInner, visitedInner)

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
mazeToString :: S.Set Edge -> (Int, Int) -> [Char]
mazeToString edges (width, height) =
  let mazeStr = do
        y <- [height, height-1 .. 0]
        x <- [0..width]
        (dir,open,closed) <- [(Loc (0, -1), ' ', '_'), (Loc (1, 0), '_', '|')]
        let nl = if x == width && dir == Loc (1,0) then "\n" else ""
        let wall = if x == 0 && dir == Loc (0,-1) then " |" else ""
        let c = if (S.member (Loc (x,y), Loc (x,y) <> dir)  edges) || (S.member (Loc (x,y) <> dir, Loc (x,y)) edges) then [open] else [closed]
        wall <> c <> nl
      top = " " <> replicate (CountOf (width*2)+1) '_' <> "\n"
  in
    top <> mazeStr
  
main :: IO ()
main = do
  [widthStr, heightStr] <- getArgs
  let width :: Int = read (toList widthStr)
  let height :: Int = read (toList heightStr)
  let boundary = (width,height)
  -- Start with an edge from below the maze into the bottom
  -- left corner. This is the entry point!
  let startEdges = (S.singleton (Loc (0,-1), Loc (0,0)))
  -- Generate the maze
  let m = maze (Loc (0,0)) boundary startEdges (S.singleton (Loc (0,0)))
  rand <- R.getStdGen
  let (edges, _) = St.evalState m rand
  -- Print it out.
  putStrLn $ fromList $ mazeToString edges boundary
