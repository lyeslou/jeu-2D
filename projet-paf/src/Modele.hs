module Modele where

import Carte
import Environnement
import Keyboard
import System.Random

import qualified Data.List as L

data Modele = Cont {carte :: Carte,
                    envi :: Envi,
                    gene :: StdGen,
                    log :: String,
                    keyboard :: Keyboard
                    }

data Ordre = N | S | E | O | U | R deriving Show

initModel :: Carte-> StdGen -> Keyboard -> Modele
initModel carte gen keyboard = Cont carte (initEnvi carte) gen "" keyboard

bouge :: Modele -> Entite -> Coord -> Modele
bouge model@ (Cont c envi g l k) entite coord =  
    (Cont c (bouge_id (id_entite entite) coord envi)  g l k)

prevoit_vache :: Modele -> Entite -> [(Int, Ordre)]
prevoit_vache (Cont carte env _ _ _) entite =
    case (trouve_id (id_entite entite) env) of
         Nothing -> []
         Just (coord, _) -> let liste = getAdjCase coord carte
           in
           L.foldl (\res x -> res ++ genererOrdre x coord env) [(1, R)] liste

genererOrdre :: (String, Maybe Case)-> Coord -> Envi-> [(Int, Ordre)]
genererOrdre (_, Nothing) _ _= []
genererOrdre (_, Just Mur) _ _ = []
genererOrdre (_, Just (Porte _ _)) _ _ = []
genererOrdre ("Sud", Just _) (C x y) env= 
    if (franchissable_env (C x (y+1)) env ) then [(1, S)] else []
genererOrdre ("Nord", Just _) (C x y) env= 
    if (franchissable_env (C x (y-1)) env ) then [(1, N)] else []
genererOrdre ("Est", Just _) (C x y) env= 
    if (franchissable_env (C (x+1) y) env ) then [(1, E)] else []
genererOrdre ("Ouest", Just _) (C x y) env= 
    if (franchissable_env (C (x-1) y) env ) then [(1, O)] else []

decide :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decide ordres model@(Cont carte env gen _ _) entite =
    case (trouve_id (id_entite entite) env) of
         Nothing -> model
         Just ((C x y), _) -> 
            let (ind,_) = (randomR (0, ((length ordres)-1) ) gen )
                (_, ord) = ordres !! ind in
                 case ord of
                      S -> bouge model entite (C x (y+1))
                      N -> bouge model entite (C x (y-1))
                      O -> bouge model entite (C (x-1) y)
                      E -> bouge model entite (C (x+1) y)
                      otherwise -> model
         



    