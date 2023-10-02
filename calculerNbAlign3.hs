
retirerTeteListe :: [Int] -> [Int]
retirerTeteListe [] = []
retirerTeteListe (x:xs) = xs


recuperer2Elements :: [Int] -> [Int]
recuperer2Elements [x,y,_] = [x,y]
recuperer2Elements (x:xs) = [x,head xs]



retirerSes2Elements :: [Int] -> [Int]
retirerSes2Elements [_] = error "liste vide"
retirerSes2Elements (_:_:xs) = xs

{--
compte :: [Int] -> Int
compte [] = 0
compte liste = do
    let x1y1 = recuperer2Elements liste
    let reste = retirerSes2Elements liste
    comptePourChacun x1y1 liste + compte reste


comptePourChacun :: [Int] -> [Int] -> Int
comptePourChacun listxy listeComplete = do

    let x2y2 = recuperer2Elements listeComplete
    let x2 = head listeComplete 
    let y2 = tail listeComplete
    let updateListe = retirerSes2Elements listeComplete
    
    let x1 = head listxy
    let y1 = tail listxy
    
    if estUneLigne x1 y1 x2 y2
        then do 
            let listex3y3 = recuperer2Elements updateListe
            let x3 = head listex3y3
            let y3 = tail listex3y3 
            let updateListe1 = retirerSes2Elements updateListe
            
            if estUneLigne x2 y2 x3 y3
                then do
                    if estUneLigneComplete x1 y1 x3 y3
                        then 1 + comptePourChacun listxy updateListe1
                    else comptePourChacun listxy updateListe1
            else comptePourChacun listxy updateListe1
    else comptePourChacun listxy updateListe
    
    --}



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




main = do
    putStrLn "Hello World"
    let test = [1,2,3,4,5]
    
    let new = recuperer2Elements test
    print new 
    print test
    
    let update = retirerSes2Elements test
    print update
    
    let testAlign = [0,0,1,1,2,2,1,0,0,3,2,0]
    
    --let count = compte testAlign
    --print count
    
    let xy = [0,1]
    let x = head xy
    let y = tail xy--ici ca retorune une liste, et non un element
    
    print x
    print y
    
    
    
    
