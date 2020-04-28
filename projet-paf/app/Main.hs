{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Control.Monad.IO.Class (MonadIO)
import System.Random as Rand

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T


import qualified System.IO as I
import Carte
import Environnement
import Modele as M
 

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')



main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.jpg" TM.createTextureMap SM.createSpriteMap
  
  
  handle  <- I.openFile "app/carte" I.ReadMode
  contenu <- I.hGetContents handle
  
  let carte = (read contenu :: Carte)    --putStr ( show((readPoly contenu) )++ "\n"++show((readPoly contenu2))++ "\n")
  I.hClose handle
  
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- initialisation du generateur
  gene <- Rand.getStdGen
  

  
  -- initialisation de l'état du jeu
  let gameModel = initModel carte gene kbd

  
  
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameModel 

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> M.Modele -> IO ()
gameLoop frameRate renderer tmap smap kbd gameModel  = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd

  
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)

  
 
  
  
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state

  --let gameState' = M.gameStep gameState kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameModel )

   