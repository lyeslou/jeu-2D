
module Model where

import SDL

import Data.Set (Set)
import qualified Data.Set as S

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int}
  deriving (Show)

data VirusState = VirusState { virusX :: Int
                             , virusY :: Int
                             , isDead :: Bool}

initVirusState :: VirusState
initVirusState = VirusState 50 50 False


initGameState :: GameState
initGameState = GameState 200 300 4 

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyboard _ =foldl(\gs x-> if x == KeycodeZ then moveUp gs
                                           else if x == KeycodeS then moveDown gs
                                           else if x == KeycodeD then moveRight gs
                                           else if x == KeycodeQ then moveLeft gs
                                           else gs) gstate keyboard
                                




                                 

