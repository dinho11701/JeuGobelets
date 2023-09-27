calculerAlignements :: [Int] -> Int
calculerAlignements [] = 0
calculerAlignements [_] = 0
calculerAlignements [_, _] = 0
calculerAlignements [_, _, _] = 0
calculerAlignements (x1 : y1 : x2 : y2 : xs) =
  if estUneLigne x1 y1 x2 y2
    then 1 + calculerAlignements (x1 : y1 : xs)
  else calculerAlignements (x1 : y1 : xs)
  


calculerAlignement3 :: [Int] -> Int
calculerAlignement3 [] = 0
calculerAlignement3 [_] = 0
calculerAlignement3 [_, _] = 0
calculerAlignement3 [_, _, _] = 0
calculerAlignement3 liste@(x1 : y1 : x2 : y2 : x3 : y3 : xs) = 
    let auMoins2AutreElem = length xs >= 2
    if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 x3 y3 
        then if estUneLigneComplete x1 y1 x3 y3 
            then if auMoins2AutreElem 
                then 1 + calculerAlignement3 (x1 : y1 : xs)
            else 1
        else 1 + calculerAlignement3 (x1 : y1 : xs)
    else calculerAlignement3 (x1 : y1 : xs)


estUneLigneComplete :: Int -> Int -> Int -> Int -> Bool
estUneLigneComplete x1 y1 x2 y2 = estUneLigneCompleteDiag x1 y1 x2 y2 || estUneLigneCompleteDroite x1 y1 x2 y2
    || estUneLigneCompleteBas x1 y1 x2 y2
    
estUneLigneCompleteDiag :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteDiag x1 y1 x2 y2 = x2 - x1 == 2 && y2 - y1 == 2

estUneLigneCompleteDroite :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteDroite x1 y1 x2 y2 = x2 - x1 == 2 && y2 - y1 == 0

estUneLigneCompleteBas :: Int -> Int -> Int -> Int -> Bool
estUneLigneCompleteBas x1 y1 x2 y2 = x2 - x1 == 0 && y2 - y1 == 2




estUneLigne :: Int -> Int -> Int -> Int -> Bool
estUneLigne x1 y1 x2 y2 =
  (estUneLigneXDroite x1 x2 && estUneLigneYDroite y1 y2)
    || (estUneLigneXBas x1 x2 && estUneLigneYBas y1 y2)
    || (estUneLigneDiagonale x1 x2 y1 y2) || (estUneLigneXDroite x1 x2 && estUneLigneYDroite y1 y2)
    && (estUneLigneXBas x1 x2 && estUneLigneYBas y1 y2)
    && (estUneLigneDiagonale x1 x2 y1 y2) 

estUneLigneXDroite :: Int -> Int -> Bool
estUneLigneXDroite x1 x2 = x1 + 1 == x2

estUneLigneYDroite :: Int -> Int -> Bool
estUneLigneYDroite y1 y2 = y2 == y1

estUneLigneXBas :: Int -> Int -> Bool
estUneLigneXBas x1 x2 = x1 == x2

estUneLigneYBas :: Int -> Int -> Bool
estUneLigneYBas y1 y2 = y1 + 1 == y2

estUneLigneDiagonale :: Int -> Int -> Int -> Int -> Bool
estUneLigneDiagonale x1 x2 y1 y2 = x1 + 1 == x2 && y1 + 1 == y2

main :: IO ()
main = do
  let tableau = [0,0, 1,1, 0,1 ,0,2 ,0,3]  -- Remplacez par votre propre tableau d'entiers ici alignements de 2
  
  let tab = [0,0,1,1,1,0,0,1]  --bon
  
  let tab1 = [0,0 ,1,0, 3,0 ,0,1 ,1,1, 2,2 , 0,3, 1,3 ,3,3]-- pour l'instant on va dire que cest bon

  
  let tab11score = [0,0,1,0,2,0,3,0,0,1,0,2,0,3,1,3,2,3,3,3,1,2,2,2,2,3,1,1,1,2,1,3]
  
  let tabDiag = [0,0,1,1]
  
  --let alignements = calculerAlignements tab1
  
  let alignement3 = calculerAlignement3 tabDiag

  putStrLn $ "Nombre d'alignements3 : " ++ show alignement3
