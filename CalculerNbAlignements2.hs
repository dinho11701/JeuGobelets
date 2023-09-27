calculerAlignements :: [Int] -> Int
calculerAlignements [] = 0
calculerAlignements [_] = 0
calculerAlignements [_, _] = 0
calculerAlignements [_, _, _] = 0
calculerAlignements (x1 : y1 : x2 : y2 : xs) =
  if estUneLigne x1 y1 x2 y2
    then 1 + calculerAlignements (x1 : y1 : xs)
  else calculerAlignements (x1 : y1 : xs)
    





estUneLigne :: Int -> Int -> Int -> Int -> Bool
estUneLigne x1 y1 x2 y2 =
  (estUneLigneXDroite x1 x2 && estUneLigneYDroite y1 y2)
    || (estUneLigneXBas x1 x2 && estUneLigneYBas y1 y2)
    || (estUneLigneDiagonale x1 x2 y1 y2)

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
  let tableau = [0,0, 1,1, 0,1 ,0,2 ,0,3]  -- Remplacez par votre propre tableau d'entiers
  
  let tab = [0,0,1,1,1,0]  

  let alignements = calculerAlignements tab

  putStrLn $ "Nombre d'alignements : " ++ show alignements
