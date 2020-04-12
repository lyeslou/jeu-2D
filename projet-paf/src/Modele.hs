module Modele where

import Carte
import Environnement
import Keyboard
import System.Random

data Modele = Cont {carte :: Carte,
                    envi :: Envi,
                    gene :: StdGen,
                    log :: String,
                    keyboard :: Keyboard
                    }

bouge :: Modele -> Entite -> Coord -> Modele
bouge model@ (Cont c envi g l k) entite coord =  
    (Cont c (bouge_id (id_entite entite) coord envi)  g l k)
    