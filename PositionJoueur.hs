-- Fonction pour trouver les positions d'un joueur avec des pièces spécifiques dans un tableau 2D
-- Renvoie une liste d'entiers représentant les positions (ligne, colonne) des pièces du joueur.
trouverPositionsJoueur :: [[String]] -> [String] -> [Int]
trouverPositionsJoueur grid piecesJoueur =
  concatMap (\(row, col) -> [row, col]) positions
  where
    positions = [(row, col) | (row, ligne) <- enumerate grid, (col, piece) <- enumerate ligne, piece `elem` piecesJoueur]
    enumerate = zip [0..]





--faire bon de 2
calculerAlignement :: [Int] -> Int -> Int -> Int
calculerAlignement liste i listeAlignement = do
    if i < length liste
        then do
            let elemy = liste !! i
            let elem1y = liste !! i + 2
            let elemx = (liste !! i + 1) + 1
            let elem1x = (liste !! i + 3)
            
            if elemx == elem1x && elemy == elem1y
                let alignement2 = 1
                listeAlignement = alignement2
            else do
                let j = i + 3 + 2
                if j < length liste
                    then do
                        let elemx = (liste !! i + 1) + 1
                        let elem1x = (liste !! j)
                        if elemx == elem1x && elemy == elem1y
                            alignement2 = 1
                            listeAlignement = alignement2
                        else
                            let j = i + 5
                            calculerAlignement liste j + 2 listeAlignement
            else
                calculerAlignement liste i + 2 listeAlignement
                
    else
        listeAlignement
        
        
        
        


main :: IO ()
main = do
  let grid =
        [ ["X3", "__", "X3", "__"],
          ["O3", "X2", "__", "X2"],
          ["__", "__", "X1", "__"],
          ["__", "__", "__", "__"]
        ]

  let piecesUser = ["X3", "X2", "X1"] -- Remplacez par les pièces du joueur que vous recherchez
  let piecesOrdi = ["O3", "O2", "O1"]

  let positions = trouverPositionsJoueur grid piecesUser
  let positionOrdi = trouverPositionsJoueur grid piecesOrdi

  putStrLn "Positions des pièces du joueurX :"
  print positions
  
  putStrLn "Positions des pièces du joueurO :"
  print positionOrdi
  
  --let listeVide = []     -- Créez une liste vide
  --let nouvelleListe = 42 : listeVide  -- Ajoutez la valeur 42 à la liste vide
  
  
  
  
  

