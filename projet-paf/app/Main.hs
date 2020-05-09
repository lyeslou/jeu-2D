{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import qualified Data.Map.Strict as M

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
import Modele 
 

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 600 600)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadBloc :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBloc rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "bloc") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "bloc") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "bloc") sprite smap
  return (tmap', smap')

loadDepart :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadDepart rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "depart") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "depart") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "depart") sprite smap
  return (tmap', smap')

loadSortie :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSortie rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "drapeau") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "drapeau") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "drapeau") sprite smap
  return (tmap', smap')


loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadNormal :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadNormal rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "normale") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "normale") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "normale") sprite smap
  return (tmap', smap')

loadPorteEO :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEO rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porteEO") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porteEO") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "porteEO") sprite smap
  return (tmap', smap')

loadPorteNS :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteNS rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "porteNS") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "porteNS") (S.mkArea 0 0 60 60)
  let smap' = SM.addSprite (SpriteId "porteNS") sprite smap
  return (tmap', smap')




main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assert/background.jpg" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadBloc renderer "assert/bloc.png" tmap smap
  (tmap'', smap'') <- loadDepart renderer "assert/depart.png" tmap' smap'
  (tmapf, smapf) <- loadSortie renderer "assert/drapeau.png" tmap'' smap''
  (tmapf',smapf') <- loadPerso renderer "assert/marioArretDroite.png" tmapf smapf
  (tmapf'', smapf'') <- loadNormal renderer "assert/normal.png" tmapf' smapf'
  (tmapfPrefinal, smapfPrefinal) <- loadPorteEO renderer "assert/porteEO.png" tmapf'' smapf''
  (tmapfFinal, smapfFinal) <- loadPorteNS renderer "assert/porteNS.png" tmapfPrefinal smapfPrefinal
  
  
  
  handle  <- I.openFile "app/carte" I.ReadMode
  contenu <- I.hGetContents handle
  
  let carte = (read contenu :: Carte)    --putStr ( show((readPoly contenu) )++ "\n"++show((readPoly contenu2))++ "\n")
  --I.hClose handle

  
  
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- initialisation du generateur
  gene <- Rand.getStdGen
   
  -- initialisation de l'état du jeu
  let gameModel = initModel carte gene kbd
  putStr (show carte)
  -- lancement de la gameLoop
  gameLoop 60 renderer tmapfFinal smapfFinal kbd gameModel carte

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Modele -> Carte-> IO ()
gameLoop frameRate renderer tmap smap kbd gameModel carte  = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd

  
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)

  M.foldrWithKey (\(C x y)  v  acc ->
      case v of 
        Mur ->do 
            acc  
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "bloc") smap)
                                 ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
        Sortie ->do
            acc  
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "drapeau") smap)
                                ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
        Entree ->do
            acc 
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "depart") smap)
                                 ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
        Porte NS Fermee ->do
              acc
              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porteNS") smap)
                                 ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
        Porte EO Fermee ->do
              acc
              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "porteEO") smap)
                                ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
        otherwise ->do
              acc
              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "normale") smap)
                                ( fromIntegral (x*60) ) 
                                 ( fromIntegral (y*60)))  
        
    ) (putStrLn"affichage de la carte") (carte_contenu carte)

  
 
  
  
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
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameModel carte)

   