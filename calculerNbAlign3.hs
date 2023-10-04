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


creerTriplet :: [Int] -> [Int] -> Int -> [Int]
creerTriplet couple listeComplete i = do
    let x1 = couple !! 0
        y1 = couple !! 1
        x2 = couple !! 2
        y2 = couple !! 3
    
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x3 = listeComplete !! i
                y3 = listeComplete !! (i + 1)
            
            if estUneLigne x2 y2 x3 y3 && estUneLigneComplete x1 y1 x3 y3
                then [x1,y1,x2,y2,x3,y3]
            else creerTriplet couple listeComplete (i + 2)
    else couple



retourneSonX2Y2 :: [Int] -> [Int] -> Int -> [Int]
retourneSonX2Y2 x1y1 listeNouvelle i = do
    if length listeNouvelle == 2
        then []
    else if i < length listeNouvelle && (i + 1) < length listeNouvelle
        then do
            let x2 = listeNouvelle !! i
                y2 = listeNouvelle !! (i + 1)
                x1 = x1y1 !! 0
                y1 = x1y1 !! 1
            if estUneLigne x1 y1 x2 y2
                then [x2,y2]
            else retourneSonX2Y2 x1y1 listeNouvelle (i + 2)
    else []


compteAvecListeComplete :: [Int] -> [Int] -> [Int] -> [Int]
compteAvecListeComplete [] _ listeCompteur = listeCompteur
compteAvecListeComplete _ [] listeCompteur = listeCompteur
compteAvecListeComplete (x1:y1:rest) listeComplete listeCompteur =
    let new = comptePourChacunFinal [x1, y1] listeComplete listeCompteur
    in compteAvecListeComplete rest listeComplete new


comptePourChacunFinal :: [Int] -> [Int] -> [Int] -> [Int]
comptePourChacunFinal listexy listeComplete listeCompteur = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        x2y2 = retourneSonX2Y2 listexy listeComplete 0
        
    if length x2y2 == 0
        then ajouteValeurInListe listeCompteur 0
    else do
        let x2 = x2y2 !! 0
            y2 = x2y2 !! 1
            couple = [x1,y1,x2,y2]
            triplet = creerTriplet couple listeComplete 0
            
        if length triplet == 6
            then ajouteValeurInListe listeCompteur 1
        else ajouteValeurInListe listeCompteur 0 
        



main :: IO ()
main = do

    let chaud = [2,2,5,5,8,8,2,2,3,4]
    let res = creerTriplet [0,0,1,1] chaud 0
    print res
    
    let count = comptePourChacunFinal [0,0] chaud []
    print count
    
    let test2 = [0,0,4,4,1,1,1,2,1,3,7,7,2,2,0,1,0,2]
    let goo = compteAvecListeComplete test2 test2 []
    
    print goo
    
    
