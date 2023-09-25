import Tableau
import Move
import Joueur
import Control.Monad (forM_)
import Data.IORef -- Importez Data.IORef pour utiliser IORef

jeu1 =
  [ ["X", "__", "__", "__"],
    ["03", "X", "__", "__"],
    ["__", "__", "X", "__"],
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


-- Création d'une liste vide
casesInterdites :: [a]
casesInterdites = []

-- Vérification si une liste est vide
estVide :: [a] -> Bool
estVide [] = True
estVide _  = False




parcourirTab2D1 :: [[String]] -> IO ()
parcourirTab2D1 rows = do
  let indexedRows = zip [0..] rows  -- Associer chaque ligne à son indice
  mapM_ (\(i, row) -> parcourirLigne i row) indexedRows

parcourirLigne :: Int -> [String] -> IO ()
parcourirLigne rowIndex cells = do
  let indexedCells = zip [0..] cells  -- Associer chaque cellule à son indice
  mapM_ (\(colIndex, cell) -> putStrLn $ "Case (" ++ show(rowIndex) ++ ", " ++ show(colIndex) ++ "): " ++ cell) indexedCells





parcourirTab2D :: [[String]] -> IORef [Int] -> IO ()
parcourirTab2D tab2D listeRef = do
  -- Lisez la liste depuis la référence
  liste2 <- readIORef listeRef
  let vide = estVide liste2

  forM_ [0..(length tab2D - 1)] $ \i -> do
    forM_ [0..(length (tab2D !! 0) - 1)] $ \j -> do
      {-if not vide
        then do
          let elem1 = (tab2D !! (maListe !! 0) !! (maListe !! 1))
          let elem2 = (tab2D !! (maListe !! 2) !! (maListe !! 3))
          if (tab2D !! j !! i) /= elem1 && (tab2D !! j !! i) /= elem2
            then do 
              putStrLn "YEEES INTERDIT"
          else putStrLn "YEEES INTERDIT"
      else putStrLn "vide"
    -}
      let element = tab2D !! i !! j
      putStrLn $ "Élément à la position (" ++ show j ++ ", " ++ show i ++ ") : " ++ show element
      
      let user = estUnUser element
      let num = 0
      print user

      if user
        then do
          let num' = incrementeNumPieceJoueur num
          let casse = faireCasDroite tab2D element j i
          if casse !! 0 == -1
            then do
              let casse1 = faireCasBas tab2D element j i
              if casse1 !! 0 == -1
                then do
                  let casse2 = faireCasDiag tab2D element j i
                  if casse2 !! 0 == -1
                    then putStrLn "Rien à proximité"
                    else do
                      let num'' = incrementeNumPieceJoueur num'
                      let casse22 = faireCasDiag tab2D element (j + 1) (i + 1)
                      if casse22 !! 0 == -1
                        then putStrLn "Alignement2"
                        else do
                          putStrLn "Alignement3"
                           -- Mettez à jour la liste globale
                          writeIORef listeRef [casse22 !! 0, casse22 !! 1, j + 1, i + 1]
                          listeMiseAJour <- readIORef listeRef
                          putStrLn $ "Valeur de num' mise à jour : " ++ show num''
                          print listeMiseAJour
                          print (tab2D !! (listeMiseAJour !! 0) !! (listeMiseAJour !! 1))
                          print (tab2D !! (listeMiseAJour !! 2) !! (listeMiseAJour !! 3))
 
              else if casse1 /= [-1, -1]
                then do 
                  let num'' = incrementeNumPieceJoueur num'
                  let casse11 = faireCasBas tab2D element j (i + 1)
                  if casse11 !! 0 == -1
                    then putStrLn "Alignement2"
                    else putStrLn "Alignement3"
                else putStrLn "chaud"
          else if casse /= [-1, -1]
            then do
              let num'' = incrementeNumPieceJoueur num'
              let casse11 = faireCasBas tab2D element (j + 1) i
              if casse11 !! 0 == -1
                then putStrLn "Alignement2"
                else putStrLn "Alignement3"
            else putStrLn "gogo"
        else putStrLn "Non"
            

-- Exemple d'utilisation
main :: IO ()
main = do
  putStrLn "debut vrai projet"

  let liste = ["X3","X2","X1"]

  let list1 = []

  let estUser = estUnUser "0"
  putStrLn (show estUser)
  
  listeRef <- newIORef []

  --parcourirTab2D jeu1 listeRef 
  --print listeRef
  
  parcourirTab2D1 jeu1
  
  
  
  
  
  
  
