module Modele where

import SDL as S

import Carte
import Environnement
import Keyboard
import System.Random
import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Data.List as L

data Modele = Cont {carte :: Carte,
                    envi :: Envi,
                    gene :: StdGen,
                    log :: String,
                    keyboard :: K.Keyboard
                    }

data Ordre = N | S | E | O | U | R deriving Show




initModel :: Carte-> StdGen -> Keyboard -> Modele
initModel carte gen keyboard = Cont carte (initEnvi carte) gen "" keyboard

bouge :: Modele -> Entite -> Coord -> Modele
bouge model@ (Cont carte envi g l k) entite coord =
    case getCase coord carte  of
        Nothing -> model
        Just Mur -> model
        Just (Porte _ Fermee) -> model
        _ -> (Cont carte (bouge_id (id_entite entite) coord envi)  g l k)
    

tour :: Modele -> Entite -> Modele
tour mod v@(Vache _ _ ) = tour_vache mod v
tour mod (Joueur _ _ ) = mod
tour mod _ = mod

tour_joueur :: Modele -> Modele
tour_joueur modele@ (Cont carte env gen log kb) = 
    case (trouve_id 1 env) of
         Nothing -> modele
         Just(coord@(C x y),entite)->
             foldl(\nmodele k-> if k == KeycodeZ 
                                then bouge nmodele entite (C x (y-1))
                                else if k == KeycodeS then bouge nmodele entite (C x (y+1))
                                else if k == KeycodeD then bouge nmodele entite (C (x+1) y)
                                else if k == KeycodeQ then bouge nmodele entite (C (x-1) y)
                                else if k == KeycodeH then utiliser_portes nmodele coord
                                else nmodele) modele kb

utiliser_portes :: Modele -> Coord -> Modele
utiliser_portes modele coord@(C x y) =
    L.foldl(\m@(Cont carte _ _ _ _) cord ->
        case utiliserPorte cord carte of
             Nothing -> m
             Just c-> m{carte=c} ) 
            modele [(C (x+1) y), (C (x-1) y), (C x (y-1)), (C x (y+1))]

gagner :: Modele -> Bool
gagner modele@(Cont carte env gen log kb) = 
    (getCoordJoueur env) ==  (pure (getCoordSortie carte) ) 
                            


tour_vache :: Modele -> Entite -> Modele
tour_vache modele entite = 
    let ordres = prevoit_vache modele entite in
        decide [(1,S),(1,N),(1,O),(1,Modele.E)] modele entite

prevoit_vache :: Modele -> Entite -> [(Int, Ordre)]
prevoit_vache (Cont carte env _ _ _) entite =
    case (trouve_id (id_entite entite) env) of
         Nothing -> []
         Just (coord, _) -> let liste = getAdjCase coord carte
           in
           L.foldl (\res x -> res ++ genererOrdre x coord env) [] liste

genererOrdre :: (String, Maybe Case)-> Coord -> Envi-> [(Int, Ordre)]
genererOrdre (_, Nothing) _ _= []
genererOrdre (_, Just Mur) _ _ = []
genererOrdre (_, Just (Porte _ _)) _ _ = []
genererOrdre ("Sud", Just _) (C x y) env= 
    if (franchissable_env (C x (y+1)) env ) then [(1, S)] else []
genererOrdre ("Nord", Just _) (C x y) env= 
    if (franchissable_env (C x (y-1)) env ) then [(1, N)] else []
genererOrdre ("Est", Just _) (C x y) env= 
    if (franchissable_env (C (x+1) y) env ) then [(1, Modele.E)] else []
genererOrdre ("Ouest", Just _) (C x y) env= 
    if (franchissable_env (C (x-1) y) env ) then [(1, O)] else []

decide :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decide ordres model@(Cont carte env gen _ _) entite =
    case (trouve_id (id_entite entite) env) of
         Nothing -> model
         Just ((C x y), _) -> 
            let (ind,g) = (randomR (0, 3) gen )
                (_, ord) = ordres !! ind in
                 case ord of
                      S -> bouge model{gene=g} entite (C x (y+1))
                      N -> bouge model{gene=g} entite (C x (y-1))
                      O -> bouge model{gene=g} entite (C (x-1) y)
                      Modele.E -> bouge model{gene=g} entite (C (x+1) y)
                      otherwise -> model{gene=g}
         



    