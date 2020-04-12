module Environnement where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Carte



data Entite = Vache {iden :: Int, pvie :: Int} 
            | Joueur {iden :: Int, pvie :: Int}

data Envi = Envi {contenu_envi :: M.Map Coord [Entite]}


trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id id (Envi contenu) = 
    M.foldlWithKey(\res coord liste -> 
        case res of 
        Just _ -> res 
        _ -> case (L.find(\entite -> (id_entite entite) == id ) liste ) of
            Just e -> (Just (coord,e))
            _ ->  Nothing
    )Nothing contenu   

id_entite :: Entite -> Int
id_entite (Vache id _)  = id
id_entite (Joueur id _) = id

remouve_env_id :: Int -> Envi -> Envi
remouve_env_id id (Envi contenu) = 
     let newmap = M.foldlWithKey(\res coord liste -> 
                case (L.find(\entite -> (id_entite entite) == id ) liste ) of
                Just e -> M.insert coord (L.delete e liste) res
                _ ->  M.insert coord liste res
                ) (M.empty) contenu  
    in
        Envi newmap

bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id id newCoord envi =
    case (trouve_id id envi) of
        Just (_,entite) -> let (Envi contenu) = remouve_env_id id envi in
                         (Envi (M.insertWith (++) newCoord [entite] contenu) )
        Nothing -> envi
    







instance Show Envi where
    show (Envi contenu) = show contenu

instance Show Entite where
    show (Vache id pv) = show "vache " ++ show id ++ show " " ++ show pv
    show (Joueur id pv) = show "joueur " ++ show id ++ show " " ++ show pv

instance Eq Entite where
    (==) e1 e2 = (id_entite e1) == (id_entite e2) 



--ex = M.fromList [((C 1 1),[Vache 1 0,Vache 2 0]),((C 1 2),[Vache 3 0, Joueur 4 0])]
--env1 = Envi ex