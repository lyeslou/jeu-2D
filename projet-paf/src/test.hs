data Entite = Vache Int
            | Joueur Int


func :: Entite -> Int
func e = case e of
              (_ a) -> a