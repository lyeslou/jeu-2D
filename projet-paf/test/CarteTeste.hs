import Carte

inv_coord_dans_rectangle :: Carte -> Bool
inv_coord_dans_rectangl (l h carte) =
    foldlWithKey (\res key (x,y) -> res && (x <= l) && (y <= h)) True carte