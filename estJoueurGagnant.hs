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

supprimerElement :: Eq a => a -> [a] -> [a]
supprimerElement _ [] = []  -- Si la liste est vide, il n'y a rien à supprimer
supprimerElement element (x:xs)
    | element == x = supprimerElement element xs  -- Si l'élément est égal à la tête de la liste, on le saute
    | otherwise = x : supprimerElement element xs 
    

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

sontSurLaMemeLigne :: Int -> Int -> Bool
sontSurLaMemeLigne y1 y2 = y1 == y2

ajouteElementALaFin :: [Int] -> Int -> [Int]
ajouteElementALaFin liste nb = liste ++ [nb]


estUneLigneDe4 :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4 x1 y1 x4 y4 = estUneLigneDe4Droite x1 y1 x4 y4 || estUneLigneDe4Bas x1 y1 x4 y4 || estUneLigneDe4Diag x1 y1 x4 y4

estUneLigneDe4Droite :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Droite x1 y1 x4 y4 = x4 - x1 == 3 && y4 - y1 == 0

estUneLigneDe4Bas :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Bas x1 y1 x4 y4 = x4 - x1 == 0 && y4 - y1 == 3

estUneLigneDe4Diag :: Int -> Int -> Int -> Int -> Bool
estUneLigneDe4Diag x1 y1 x4 y4 = x4 - x1 == 3 && y4 - y1 == 3



supprimerElementALaPosition :: Int -> [a] -> [a]
supprimerElementALaPosition _ [] = []
supprimerElementALaPosition 0 (_:xs) = xs
supprimerElementALaPosition n (x:xs)
    | n < 0 = x:xs  -- Si la position est négative, la liste reste inchangée
    | otherwise = x : supprimerElementALaPosition (n - 1) xs



data ListeCoord = ListeCoord [Int] [Int]

pieceJoueur = ["X3", "X2", "X1", "X0"]
pieceOrdi = ["O3", "O2", "O1", "O0"]

--existeUnAlignementde4Pieces :: [Int] -> [Int] = do


creerListeCoordPieces :: [[String]] -> [Int] -> [Int] -> Int -> Int -> ListeCoord
creerListeCoordPieces jeu pieceJr pieceCom i j = do
    if i < length jeu
        then do
            if j < length jeu
                then do
                    let casee = jeu !! i !! j
                        casseConverti = show casee
                        
                    if casseConverti /= "__"
                        then do 
                            if casseConverti `elem` pieceJoueur
                            then do
                                let newPieceJr = ajouteElementALaFin pieceJr i
                                    newUpdate = ajouteElementALaFin newPieceJr j
                                creerListeCoordPieces jeu newUpdate pieceCom i (j + 1)
                            else if casseConverti `elem` pieceOrdi
                                then do 
                                    let newPieceCom = ajouteElementALaFin pieceCom i
                                        newUpdate = ajouteElementALaFin newPieceCom j
                                    creerListeCoordPieces jeu pieceJr newUpdate i (j + 1)
                            else 
                                creerListeCoordPieces jeu pieceJr pieceCom i (j + 1)
                    else creerListeCoordPieces jeu pieceJr pieceCom i (j + 1)
                    
            else creerListeCoordPieces jeu pieceJr pieceCom (i + 1) j
    else ListeCoord pieceJr pieceCom
    
    
    {--
creerListeCoordPieces :: [[String]] -> [Int] -> [Int] -> Int -> Int -> IO()
creerListeCoordPieces jeu pieceJr pieceCom i j = do
    if i < length jeu
        then do
            if j < length jeu
                then do
                    let casee = jeu !! i !! j
                        casseConverti = read casee :: Int
                    --putStrLn $ "Valeur de casee : " ++ casseConverti
                        
                    if casee /= "__"
                        then do 
                            --putStrLn $ "Case non vide : " ++ casseConverti  -- Ajoutez cette ligne pour le débogage
                            if casee `elem` pieceJoueur
                            then do
                                print "OUI"
                                let newPieceJr = ajouteElementALaFin pieceJr i
                                    newUpdate = ajouteElementALaFin newPieceJr j
                                creerListeCoordPieces jeu newUpdate pieceCom i (j + 1)
                            else if casee `elem` pieceOrdi
                                then do 
                                    let newPieceCom = ajouteElementALaFin pieceCom i
                                        newUpdate = ajouteElementALaFin newPieceCom j
                                    creerListeCoordPieces jeu pieceJr newUpdate i (j + 1)
                            else do
                                print "cest egal en theorie mais il rentre pas"
                                creerListeCoordPieces jeu pieceJr pieceCom i (j + 1)
                    else do
                        putStrLn "Case vide"  -- Ajoutez cette ligne pour le débogage
                        creerListeCoordPieces jeu pieceJr pieceCom i (j + 1)
            else creerListeCoordPieces jeu pieceJr pieceCom (i + 1) j
    else print "FINIT"

    --}


estLeJoueurGagnant :: [Int] -> Int -> Bool 
estLeJoueurGagnant listeComplete i = do
    
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x = listeComplete !! i
                y = listeComplete !! (i + 1)
                listeAlign4 = existeAlign4 [x,y] listeComplete 
        
            if length listeAlign4 == 6
                then True
            else estLeJoueurGagnant listeComplete (i + 2)
    else False
        


            


existeAlign4 :: [Int] -> [Int] -> [Int]
existeAlign4 x1y1 listeComplete  = do

    let listeTrouve = chercherAlignement4Pieces x1y1 listeComplete [] [] 0
    
    if length listeTrouve == 6
        then listeTrouve
    else if length listeTrouve == 0
        then []
    else 
        let newListeComplete = enleverElementsInutiles x1y1 listeComplete 0 0
        in existeAlign4 x1y1 newListeComplete 
        
        --enleverElementsInutiles et 
        --enleverElementsInutiles listexy listeComplete 0 0
        --refaire l'operation chercherAlignement4Pieces avec la new liste complete




--renvoie la nouvelle liste complete 
enleverElementsInutiles :: [Int] -> [Int] -> Int -> Int -> [Int]
enleverElementsInutiles listePosXY listeComplete i decalage = do

    if i < length listePosXY
        then do 
            let position = (listePosXY !! i) - decalage
                element = (listeComplete !! position)
                newListeComplete = supprimerElementALaPosition position listeComplete
            enleverElementsInutiles listePosXY newListeComplete (i + 1) (decalage + 1)
    else listeComplete
    
                  
          
                    

--retourne position xy des 4 pieces
chercherAlignement4Pieces :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> [Int]
chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY i = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        
    if i < length listeComplete && (i + 1) < length listeComplete
        then do
            let x2 = listeComplete !! i 
                y2 = listeComplete !! (i + 1)
                
            if length listeAlign4 == 0
                then do
                    if estUneLigne x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate(i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else if length listeAlign4 == 2
                then do
                    let x = listeAlign4 !! 0
                        y = listeAlign4 !! 1
                    if estUneLigne x y x2 y2 && estUneLigneComplete x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate (i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else if length listeAlign4 == 4
                then do
                    let x = listeAlign4 !! 0
                        y = listeAlign4 !! 1  
                        x3 = listeAlign4 !! 2
                        y3 = listeAlign4 !! 3
                    if estUneLigne x3 y3 x2 y2 && estUneLigneComplete x y x2 y2 && estUneLigneDe4 x1 y1 x2 y2
                        then do
                            let newListeAlign4 = ajouteElementALaFin listeAlign4 x2
                                update = ajouteElementALaFin newListeAlign4 y2
                                posXY = ajouteElementALaFin listePosXY i
                                posXYUpdate = ajouteElementALaFin posXY (i + 1)
                            chercherAlignement4Pieces listexy listeComplete update posXYUpdate (i + 2)
                    else chercherAlignement4Pieces listexy listeComplete listeAlign4 listePosXY (i + 2)
            else listePosXY
    else listePosXY
                        

jeu = [ ["O3", "", "__", "X3"]
                  , ["O2", "X3", "__", "__"]
                  , ["O1", "__", "__", "__"]
                  , ["O0", "__", "__", "__"] ]



main = do
    putStrLn "Hello World"
    
    
    let test1 = [0,0,1,0,2,0,0,1,1,1,1,2,2,2,0,3,3,3]
    
    
    --let res = chercherAlignement4Pieces [0,0] test1 [] 0
    ---print res
    ---print (res !! 0)
    --let r = estUneLigne 0 0 1 1
    --let r = estUneLigneDe4 0 0 2 5
    --print r
    
    --let l = existeAlign4 test1 test1 
    --print l

    --let liste = [1, 2, 3, 4, 5]
    --let nouvelListe = supprimerElement 3 liste
    --print nouvelListe
    

    --listeAlign4 listeComplete i j
    --let liste1 = enleverElementsInutiles [1,1,2,2] test1 0 0
    --print liste1
    
    
    let maListe = [1,1,2,3,4,5,2,3]
    --let nouvelleListe = supprimerElementALaPosition 6 maListe  -- Supprimer l'élément à la position 2 (3ème élément)
    --print nouvelleListe  -- Résultat : [1, 2, 4, 5]
    
    --let n = enleverElementsInutiles [0,1,6,7] maListe 0 0
    --print n

    --let pos = chercherAlignement4Pieces [0,0] test1 [] [] 0
    --print pos

    let pos1 = existeAlign4 [0,0] test1
    print pos1
    
    let re = estLeJoueurGagnant test1 0
    print re
    
    --let (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces jeu [] [] 0 0
    --print posPieceJr
    --print posPieceOrdi
    
    creerListeCoordPieces jeu [] [] 0 0
    
    --enleverElementsInutiles1 [1,1,2,2] test1 0 0
    
    
    
    
    
    
    
    
