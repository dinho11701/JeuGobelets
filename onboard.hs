
--meilleur version , plus clean
modifierCase2D :: [[a]] -> a -> Int -> Int -> [[a]]
modifierCase2D tableau nouvelleValeur ligne colonne
  | ligne < 0 || ligne >= length tableau || colonne < 0 || colonne >= length (tableau !! 0) = tableau
  | otherwise =
    let (avant, apres) = splitAt colonne (tableau !! ligne)
    in take ligne tableau ++ [avant ++ [nouvelleValeur] ++ tail apres] ++ drop (ligne + 1) tableau



onboard :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
onboard jeuDepart y1 x1 y2 x2
  | x1 < 0 || x1 >= length jeuDepart || y1 < 0 || y1 >= length (jeuDepart !! 0) || x2 < 0 || x2 >= length jeuDepart || y2 < 0 || y2 >= length (jeuDepart !! 0) = jeuDepart
  | otherwise =
    let pieceADeplacer = jeuDepart !! x1 !! y1
        jeuTemporaire = modifierCase2D jeuDepart pieceADeplacer x2 y2
        jeuFinal = modifierCase2D jeuTemporaire "-" x1 y1
    in jeuFinal
