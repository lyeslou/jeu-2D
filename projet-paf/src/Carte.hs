module Carte where 

import qualified Data.Map.Strict as M
import qualified Data.List as L

data PDirection = NS | EO deriving Eq

data StatutP = Ouverte | Fermee deriving Eq

data Case = Normal
    | Porte PDirection StatutP
    | Mur 
    | Entree
    | Sortie
    deriving Eq

data Coord = C {cx :: Int, cy :: Int} deriving Eq

data Carte = Carte {cartel :: Int,
                    carteh :: Int,
                    carte_contenu :: (M.Map Coord Case)
                    }

instance Ord Coord where
    compare = compareCoord

instance Show Coord where
    show (C x y)= show "(" ++ show x ++", " ++ show y ++ show ")" 



compareCoord :: Coord -> Coord -> Ordering
compareCoord (C x1 y1) (C x2 y2) | y1 < y2 = GT
                                 | y1 > y2 = LT
                                 | x1 < x2 = GT
                                 | x1 > x2 = GT
                                 | otherwise = EQ


------------ fonctionalite ----------------------

franchissable :: Coord -> Carte -> Bool
franchissable coord (Carte l h carte) = 
    case (M.lookup coord carte) of
         (Just (Porte _ _)) -> True
         _ -> False  

getCase :: Coord -> Carte -> Maybe Case
getCase  coord (Carte l h carte) = M.lookup coord carte

editCase :: Coord -> Case -> Carte -> Carte
editCase coord newcaz (Carte l h carte) = 
    Carte l h (M.insert coord newcaz carte)

ouvrirPorte :: Coord -> Carte -> Maybe Carte
ouvrirPorte coord c@(Carte l h carte) = 
    case (getCase coord c ) of
        Just (Porte d _) -> Just (Carte l h (M.insert coord (Porte d Ouverte) carte) )
        _ -> Nothing 

fermerPorte :: Coord -> Carte -> Maybe Carte
fermerPorte coord c@(Carte l h carte) = 
    case (getCase coord c ) of
        Just (Porte d _) -> Just (Carte l h (M.insert coord (Porte d Fermee) carte) )
        _ -> Nothing 

         

------------------ TESTE -------------------------------
pre_coord_dans_rectangle :: Coord -> Carte -> Bool
pre_coord_dans_rectangle (C x y) (Carte l h _) = ( x <= l ) && ( y <= h )  

inv_coord_dans_rectangle :: Carte -> Bool
inv_coord_dans_rectangle ( Carte l h carte) =
    M.foldlWithKey (\res (C x y) _ -> res && (x <= l) && (y <= h)) True carte


inv_unique_entree :: Carte -> Bool
inv_unique_entree ( Carte _ _ carte) =
    (M.foldl (\res caz -> if caz == Entree then (res + 1) else res ) 0 carte) == 1

inv_unique_sortie :: Carte -> Bool
inv_unique_sortie ( Carte _ _ carte) =
    (M.foldl (\res caz -> if caz == Sortie then (res + 1) else res ) 0 carte) == 1

inv_entouree_de_murs :: Carte -> Bool
inv_entouree_de_murs carte = (ligne_de_murVerticale carte ) && (ligne_de_murHorizontale carte)

--toutes les coord decrit par h et l sont dans la map
inv_coord :: Carte -> Bool
inv_coord (Carte l h carte) = 
    L.foldl(\res1 y -> 
        if res1 == False 
        then False 
        else (L.foldl(\res2 x -> 
            case (M.lookup (C x y) carte) of
                 (Just _) -> res2 && True
                 Nothing -> False )   True [0..l] ) )  True [0 .. h]

-- porte entouree par des murs
inv_porte_murs :: Carte -> Bool
inv_porte_murs (Carte l h carte )=
    M.foldlWithKey(\res (C x y) caz -> 
        case caz of
            Porte NS _ -> case (M.lookup (C (x-1) y) carte, (M.lookup (C (x+1) y) carte) ) of 
                                (Just Mur, Just Mur)-> True && res
                                (_,_) -> False
            Porte EO _ -> case (M.lookup (C x (y-1)) carte, (M.lookup (C x (y-1) ) carte) ) of 
                                (Just Mur, Just Mur)-> True && res
                                (_,_) -> False    
            _ -> False ) True carte


ligne_de_murVerticale :: Carte -> Bool
ligne_de_murVerticale (Carte l h carte) =
    foldl(\res y -> case (M.lookup (C 0 y) carte, M.lookup (C l y) carte ) of
                         (Just Mur,Just Mur) -> res && True
                         (_,_) -> False)  True [0..h]

ligne_de_murHorizontale :: Carte -> Bool
ligne_de_murHorizontale (Carte l h carte) =
    foldl(\res x -> case (M.lookup (C x 0) carte, M.lookup (C x h) carte ) of
                         (Just Mur,Just Mur) -> res && True
                         (_,_) -> False)  True [0..l]