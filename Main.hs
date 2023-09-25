import Tableau
import Move
import Joueur
import Control.Monad (forM_)


jeu1 =
  [ ["X3", "__", "__", "__"],
    ["03", "X2", "__", "__"],
    ["__", "__", "__", "__"],
    ["__", "__", "__", "__"]
  ]

incrementeNumPieceJoueur :: Int -> Int
incrementeNumPieceJoueur x = x + 1

faireCasDroite :: [[String]] -> String -> Int -> Int -> [Int]
faireCasDroite tab2D element j i = do
  let listePos = deplacer j i
  if (listePos !! 0) < 4 && i < 4
    then do
      let element1 = tab2D !! (listePos !! 0) !! j
      let user1 = estUnUser element1
      if user1
        then listePos
        else [-1, -1]
    else [-1, -1]

faireCasBas :: [[String]] -> String -> Int -> Int -> [Int]
faireCasBas tab2D element j i = do
  let listePos = deplacer j i
  if (listePos !! 0) < 4 && i < 4
    then do
      let element1 = tab2D !! j !! (listePos !! 0)  -- Correction de l'indice
      let user1 = estUnUser element1
      if user1
        then listePos
        else [-1, -1]
    else [-1, -1]

faireCasDiag :: [[String]] -> String -> Int -> Int -> [Int]
faireCasDiag tab2D element j i = do
  let listePos = deplacer j i
  if (listePos !! 0) < 4 && i < 4
    then do
      let element1 = tab2D !! (listePos !! 0) !! (listePos !! 1)  -- Correction des indices
      let user1 = estUnUser element1
      if user1
        then listePos
        else [-1, -1]
    else [-1, -1]

parcourirTab2D :: [[String]] -> IO ()
parcourirTab2D tab2D = do
  -- Parcourir le tableau en utilisant deux boucles for
  forM_ [0..(length tab2D - 1)] $ \i -> do
    forM_ [0..(length (tab2D !! 0) - 1)] $ \j -> do
      let element = tab2D !! i !! j
      putStrLn $ "Élément à la position (" ++ show j ++ ", " ++ show i ++ ") : " ++ show element
      let user = estUnUser element
      let num = 0
      print user
      if user
        then do
          let num' = incrementeNumPieceJoueur num  -- Mise à jour de num
          let casse = faireCasDroite tab2D element j i
          if casse !! 0 == -1
            then do
              let casse1 = faireCasBas tab2D element j i  -- Correction de la fonction appelée
              if casse1 !! 0 == -1
                then do
                  let casse2 = faireCasDiag tab2D element j i  -- Correction de la fonction appelée
                  if casse2 !! 0 == -1
                    then putStrLn "Rien à proximité"
                    else do
                      let num'' = incrementeNumPieceJoueur num'  -- Incrémentation de num
                      putStrLn $ "L'utilisateur a la case (" ++ show j ++ ", " ++ show i ++ ") : " ++ show element
                      putStrLn $ "Valeur de num' mise à jour : " ++ show num''

              else putStrLn $ "L'utilisateur a la case (" ++ show j ++ ", " ++ show i ++ ") : " ++ show element
            else putStrLn $ "L'utilisateur a la case (" ++ show j ++ ", " ++ show i ++ ") : " ++ show element
        else putStrLn "Non"


-- Exemple d'utilisation
main :: IO ()
main = do
  putStrLn "debut vrai projet"

  let liste = ["X3","X2","X1"]

  let estUser = estUnUser "0"
  putStrLn (show estUser)

  parcourirTab2D jeu1
  
  
  
  
  
  
