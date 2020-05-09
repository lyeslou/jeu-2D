import Data.Map as M
import System.Random

data Entite = Vache Int
            | Joueur Int
data Modele = Cont { gene :: StdGen}

modele = Cont (mkStdGen 5)
gen = (gene modele)
get x = randomR x gen