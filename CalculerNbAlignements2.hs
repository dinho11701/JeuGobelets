calculerAlignements2 :: [Int] -> Int
calculerAlignements2 [] = 0
calculerAlignements2 [_] = 0
calculerAlignements2 [_, _] = 0
calculerAlignements2 (x1 : x2 : xs) =
  if estAligne x1 x2
    then 1 + calculerAlignements2 (x2 : xs)
    else calculerAlignements2 (x2 : xs)

estAligne :: Int -> Int -> Bool
estAligne x1 x2 = estAligneX x1 x2 || estAligneY x1 x2

estAligneX :: Int -> Int -> Bool
estAligneX x1 x2 = abs (x1 `mod` 4 - x2 `mod` 4) == 1

estAligneY :: Int -> Int -> Bool
estAligneY x1 x2 = abs (x1 `div` 4 - x2 `div` 4) == 1


main :: IO ()
main = do
  let tableau = [0, 0, 2,2,2, 0, 1, 1, 2, 3,0,2,0,3]  -- Remplacez par votre propre tableau d'entiers

  let alignements2 = calculerAlignements2 tableau

  putStrLn $ "Nombre d'alignements de 2 points : " ++ show alignements2
