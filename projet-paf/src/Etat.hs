module Etat where 

import Modele
import Keyboard
import Carte
import Environnement
import System.Random

import qualified Data.Map.Strict as M
import qualified Data.List as L



data Etat = Perdu
    | Gagne
    | Tour { num_tour :: Int ,
        carte_tour :: Carte ,
        envi_tour :: Envi ,
        gen_tour :: StdGen ,
        obj_tour :: (M.Map Int Entite ),
        journal_tour :: String}


init_etat :: Carte -> StdGen-> Etat
init_etat carte gen = 
    let env = initEnvi carte
        objets = get_objets env
    in
        (Tour 1 carte (initEnvi carte) gen objets "")
etat_tour :: RealFrac a => Etat -> Keyboard -> a -> Etat
etat_tour  etat@(Tour num carte env gen obj jour) keyboard _ = 
    let modele = tour_joueur (Cont carte env gen jour keyboard)
        newmodele@(Cont carte' env' gen' jour' keyboard') = M.foldl (\res entite -> tour res entite) modele obj
    in  
        case gagner newmodele of
            True -> Gagne
            _ -> (Tour num carte' env' gen' obj jour')



