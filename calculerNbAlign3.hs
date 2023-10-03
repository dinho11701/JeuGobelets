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

ajouteValeurInListe :: [Int] -> Int -> [Int]
ajouteValeurInListe liste nb = nb : liste

obtenirDeuxSuivants :: Eq a => [a] -> a -> a -> Maybe (a, a)
obtenirDeuxSuivants [] _ _ = Nothing
obtenirDeuxSuivants [_] _ _ = Nothing
obtenirDeuxSuivants [_, _] _ _ = Nothing
obtenirDeuxSuivants (x:y:rest) elem1 elem2
    | x == elem1 && y == elem2 = case rest of
        (a:b:_) -> Just (a, b)
        _ -> Nothing
    | otherwise = obtenirDeuxSuivants (y:rest) elem1 elem2


trouverDebutLigne :: [Int] -> [Int] -> [Int] -> [Int]
trouverDebutLigne listex1y1 listex2y2 listeComplete =
    if length listex1y1 == 0
        then [-1]
    else
        let x2 = listex2y2 !! 0
            y2 = listex2y2 !! 1
        in
        let maybeX2Y2 = obtenirDeuxSuivants listeComplete x2 y2
        in
        case maybeX2Y2 of
            Nothing -> [-1]
            Just (x2', y2') ->
                let x1 = listex1y1 !! 0
                    y1 = listex1y1 !! 1
                in
                if estUneLigne x1 y1 x2' y2' 
                    then [x1, y1, y2', x2']
                else 
                    trouverDebutLigne listex1y1 (x2':y2':[]) listeComplete


trouverFinLigne :: [Int] -> [Int] -> [Int] -> [Int]
trouverFinLigne listex1y1x2y2 x3y3 listeComplete =
    
    if length listex1y1x2y2 == 0
        then [-1]
    else 
        let x1 = listex1y1x2y2 !! 0
            y1 = listex1y1x2y2 !! 1
            x2 = listex1y1x2y2 !! 2
            y2 = listex1y1x2y2 !! 3
        in
        if length x3y3 == 0
            then [-1]
        else
            let x3 = x3y3 !! 0
                y3 = x3y3 !! 1
            in
            if estUneLigne x2 y2 x3 y3 && estUneLigneComplete x1 y1 x3 y3
                then [x1, y1, x2, y2, x3, y3]
            else
                let maybeX3Y3 = obtenirDeuxSuivants listeComplete x3 y3
                in
                case maybeX3Y3 of
                    Nothing -> [-1]
                    Just (x3', y3') -> trouverFinLigne listex1y1x2y2 (x3' : y3' : []) listeComplete



comptePourChacun1 :: [Int] -> [Int] -> [Int] -> [Int]
comptePourChacun1 listexy listeComplete listeCompteur =

    let x1 = listexy !! 0
        y1 = listexy !! 1
        --putStrLn "x1 y1"
        --print x1
        --print y1
    in
    let x2y2 = obtenirDeuxSuivants listeComplete x1 y1
    --putStrLn "liste x2y2"
    --print x2y2
    in
    case x2y2 of
        Nothing -> [-1]
        Just (x2, y2) ->
            let pairex1y1x2y2 = trouverDebutLigne [x1, y1] [x2, y2] listeComplete
            --putStrLn "x1y1x2y2"
            --print pairex1y1x2y2
            in
            case length pairex1y1x2y2 of
                1 -> ajouteValeurInListe listeCompteur 0
                _ ->
                    let x3y3 = obtenirDeuxSuivants listeComplete x1 y1
                    --putStrLn "liste x3y3"
                    --print x3y3
                    in
                    let pairex1y1x2y2x3y3 =
                            case x3y3 of
                                Just (x3, y3) -> trouverFinLigne pairex1y1x2y2 [x3, y3] listeComplete
                                Nothing -> [-1]
                    in
                    case length pairex1y1x2y2x3y3 of
                        1 -> ajouteValeurInListe listeCompteur 0
                        _ -> ajouteValeurInListe listeCompteur 1


comptePourChacun :: [Int] -> [Int] -> [Int] -> IO ()
comptePourChacun listexy listeComplete listeCompteur = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
    {--putStrLn "Listexy:"
    print listexy
    putStrLn "x1 y1:"
    print x1
    print y1--}

    let x2y2 = obtenirDeuxSuivants listeComplete x1 y1
    --putStrLn "x2y2:"
    --print x2y2

    case x2y2 of
        Nothing -> do
            putStrLn "x2y2 est Nothing"
            -- Vous pouvez effectuer d'autres opérations IO ici si nécessaire
        Just (x2, y2) -> do
            putStrLn "x2y2 est Just"
            putStrLn "x2 y2:"
            print x2
            print y2

            let pairex1y1x2y2 = trouverDebutLigne [x1, y1] [x2, y2] listeComplete
            putStrLn "Paire x1y1x2y2:"
            print pairex1y1x2y2

            case length pairex1y1x2y2 of
                1 -> do
                    putStrLn "Longueur de la pairex1y1x2y2 est 1"
                _ -> do
                    putStrLn "Longueur de la pairex1y1x2y2 n'est pas 1"
                    let x3y3 = obtenirDeuxSuivants listeComplete x1 y1
                    putStrLn "x3y3:"
                    print x3y3

                    case x3y3 of
                        Just (x3, y3) -> do
                            putStrLn "x3y3 est Just"
                            putStrLn "x3y3:"
                            print x3y3
                            let pairex1y1x2y2x3y3 = trouverFinLigne pairex1y1x2y2 [x3, y3] listeComplete
                            putStrLn "pairex1y1x2y2 ici"
                            print pairex1y1x2y2
                            putStrLn "Paire x1y1x2y2x3y3:"
                            print pairex1y1x2y2x3y3

                            case length pairex1y1x2y2x3y3 of
                                1 -> do
                                    putStrLn "Longueur de la pairex1y1x2y2x3y3 est 1"
                                _ -> do
                                    putStrLn "Longueur de la pairex1y1x2y2x3y3 n'est pas 1"

                        Nothing -> do
                            putStrLn "x3y3 est Nothing"
                            -- Vous pouvez effectuer d'autres opérations IO ici si nécessaire





main :: IO ()
main = do
    
-- Exemple d'utilisation :
    let liste = [1, 2, 3, 4, 5, 6]
    
    let test = [0,0,3,0,5,0,1,1]
    
    case obtenirDeuxSuivants liste 5 6 of
        Just (a, b) -> putStrLn $ "Les deux éléments suivants sont : " ++ show a ++ " et " ++ show b
        Nothing -> putStrLn "Les éléments spécifiés ne se suivent pas dans la liste."
        
    
        
    --let n = comptePourChacun [0,0] [] 0
    --print n
    
    {--let paire = trouverDebutLigne [0,0] [3,0] test
    print paire
    
    let test1 = [0,0,0,1,1,0,2,2,0,2]
    
    let triplet = trouverFinLigne [0,0,1,1] [0,1] test1
    print triplet
    
    let count = comptePourChacun [0,0] test1 []
    print count--}
    
    let test1 = [0,0,0,1,1,0,2,2,0,2]
    
    let test2 = [0,0,4,4,1,1,7,7,2,2]
    
    --let triplet = trouverFinLigne [0,0,1,1] [0,1] test1
    --print triplet
    
    --let tt = (triplet !! 2)
    --print tt
    
    comptePourChacun [0,0] test1 [1]
    
    let count = comptePourChacun1 [1,1] test2 []
    print count
    
    
    
    

    
    
    
    
    
