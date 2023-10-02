
retirerTeteListe :: [Int] -> [Int]
retirerTeteListe [] = []
retirerTeteListe (x:xs) = xs


recuperer2Elements :: [Int] -> [Int]
recuperer2Elements [] = error "vide"
recuperer2Elements [x,y,_] = [x,y]
recuperer2Elements (x:xs) = [x,head xs]



retirerSes2Elements :: [Int] -> [Int]
retirerSes2Elements [_] = error "liste vide"
--retirerSes2Elements [x,y] = [x,y]
retirerSes2Elements (_:_:xs) = xs


afficheCount :: [Int] -> IO ()
afficheCount xy = print xy


{--compte :: [Int] -> [Int] -> [Int]
compte [] listeCompteur = listeCompteur
compte liste listeCompteur = 
    let x1y1 = recuperer2Elements liste
        reste = retirerSes2Elements liste
        nb = comptePourChacun x1y1 reste 0
        new = ajouteValeurInListe listeCompteur nb
    in new
--}

{--  
compte2 :: [Int] -> [Int] -> IO()
compte2 [] listeCompteur = listeCompteur
compte2 liste listeCompteur = 
    let x1y1 = recuperer2Elements liste
        --reste = retirerSes2Elements liste
        putStrLn "---------------"
        putStrLn "les x1 y1:"
        let x1 = head x1y1
        let y1 = x1y1 !! 1
        print x1
        print y1 
        nb = comptePourChacun x1y1 reste 0
        print nb
        new = ajouteValeurInListe listeCompteur nb
        reste = retirerSes2Elements liste
    in compte reste new
    --}
    

{--    
compte1 :: [Int] -> [Int] -> IO()
compte1 [] listeCompteur = do
    putStrLn "Résultat final :"
    print listeCompteur
compte1 liste listeCompteur = do
    let x1y1 = recuperer2Elements liste
    let x1 = head x1y1
    let y1 = x1y1 !! 1
    putStrLn "---------------"
    putStrLn "Les x1 y1 :"
    print x1
    print y1
    let reste = retirerSes2Elements liste
    putStrLn "---------------"
    putStrLn "Le reste :"
    print reste
    let nb = comptePourChacun x1y1 liste 0
    putStrLn "---------------"
    putStrLn "Nombre calculé :"
    print nb
    let new = ajouteValeurInListe listeCompteur nb
    putStrLn "---------------"
    putStrLn "Nouvelle liste compteur :"
    print new
    compte1 reste new

--}
  {--  let x2y2 = recuperer2Elements reste
        reste1 = retirerSes2Elements reste
        nb1 = comptePourChacun x1y1 reste1 0
        new1 = ajouteValeurInListe new1 nb
    in new1--}

    --tester manuellement pour voir le comportement 
    
    
retireEtRecupeX2Y2 :: [Int] -> [Int] -> Int
retireEtRecupeX2Y2 listex1y1 reste =
    let x1 = listex1y1 !! 0
        y1 = listex1y1 !! 1
    in
    if length reste /= 0
        then let x2y2 = recuperer2Elements reste
                 x2 = x2y2 !! 0
                 y2 = x2y2 !! 1
             in
             if estUneLigne x1 y1 x2 y2 && (length (retirerSes2Elements reste) == 0 || length (retirerSes2Elements reste) /= 0)
                 then 1
                 else if length reste /= 0
                     then retireEtRecupeX2Y2 listex1y1 (retirerSes2Elements reste)
                     else -1
        else -1

    
retireEtRecupeX3Y3 :: [Int] -> [Int] -> Int
retireEtRecupeX3Y3 listex1y1x2y2 reste = do
    let x1 = listex1y1x2y2 !! 0
    let y1 = listex1y1x2y2 !! 1
    let x2 = listex1y1x2y2 !! 2
    let y2 = listex1y1x2y2 !! 3
    
    if length reste == 2
        then do
            let x3y3 = recuperer2Elements reste
            if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 (x3y3 !! 0) (x3y3 !! 1) && estUneLigneComplete x1 y1 (x3y3 !! 0) (x3y3 !! 1)
                then 1
            else -1
            
    else do
        let x3y3 = recuperer2Elements reste
        let newReste = retirerSes2Elements reste
    
        let x3y3 = recuperer2Elements reste
        let x3 = x3y3 !! 0
        let y3 = x3y3 !! 1
        let newReste = retirerSes2Elements reste
        
        if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 x3 y3 && estUneLigneComplete x1 y1 x3 y3 && (length newReste == 0 || length newReste == 2)
            then 1
        else if length newReste /= 0
            then retireEtRecupeX3Y3 listex1y1x2y2 newReste
        else -1

{--le bon compte
compte :: [Int] -> [Int] -> [Int]
compte [] listeCompteur = listeCompteur
compte liste listeCompteur = 
    let x1y1 = recuperer2Elements liste
        nb = comptePourChacun x1y1 liste 0
        new = ajouteValeurInListe listeCompteur nb
    in compte (retirerSes2Elements liste) new
--}


comptePourChacun :: [Int] -> [Int] -> Int -> IO ()
--comptePourChacun _ [] count = count
comptePourChacun listxy listeComplete count = do

    let x1 = head listxy
    let y1 = listxy !! 1
    
    {--let x2y2 = recuperer2Elements listeComplete
    let x2 = head listeComplete 
    let y2 = listeComplete !! 1
    let updateListe = retirerSes2Elements listeComplete--}
    
    let (x2y2, x2, y2, updateListe) = if length listxy == 4
            then (recuperer2Elements listeComplete, listxy !! 2, listxy !! 3, listeComplete)
            else (recuperer2Elements listeComplete, head listeComplete, listeComplete !! 1, retirerSes2Elements listeComplete)
            
    print updateListe

    putStrLn "Les x1 y1:"
    
    print x1
    print y1
    
    putStrLn "Les x2 y2:"
    
    print x2
    print y2
    
    
    if length updateListe == 2 || length updateListe == 0
        then do--count
            print updateListe
            putStrLn "fin ici :"
            print count
    else do

        let listex3y3 = recuperer2Elements updateListe
        let updateListe1 = retirerSes2Elements updateListe
        let x3 = head listex3y3 
        let y3 = listex3y3 !! 1
        
        if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 x3 y3 && estUneLigneComplete x1 y1 x3 y3
            then do
                putStrLn "Les x3 y3:"
                print x3
                print y3
                putStrLn "valeur ici :"
                print (count + 1)
    
                comptePourChacun listxy updateListe1 (count + 1)
        else if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 x3 y3 == False
            then do
                let retour = retireEtRecupeX3Y3 [x1,y1,x2,y2] updateListe1
                if retour == 1
                    then do comptePourChacun listxy updateListe1 (count + 1)
                else comptePourChacun listxy updateListe1 count
        else if estUneLigne x1 y1 x2 y2 == False
            then do 
                print updateListe1
                let retour = retireEtRecupeX2Y2 [x1,y1] updateListe1
                if retour == 1
                    then do 
                        putStrLn "valeur ici poto:"
                        print count 
                        comptePourChacun [x1,y1,x2,y2] updateListe1 count
                else do
                    putStrLn "cest toiii heiin"
                    
                    putStrLn "yoooLes x1 y1:"
    
                    print x1
                    print y1
                    
                    putStrLn "yoooLes x2 y2:"
                    
                    print x2
                    print y2
                    
                    print updateListe1
                    comptePourChacun listxy updateListe1 count
        else if estUneLigne x1 y1 x2 y2 == True 
            then do
                putStrLn "LAAAA"
                print updateListe1
                comptePourChacun listxy updateListe1 count
        else comptePourChacun listxy updateListe1 count
        {--
        else if estUneLigne x1 y1 x2 y2 == False && estUneLigne x2 y2 x3 y3 == False && estUneLigneComplete x1 y1 x3 y3 == False
            then comptePourChacun listxy updateListe1 count
        
        else if estUneLigne x1 y1 x2 y2 == False && estUneLigne x2 y2 x3 y3 == False && estUneLigneComplete x1 y1 x3 y3 == False
        
        else if estUneLigne x2 y2 x3 y3 == False 
        
        else if estUneLigneComplete x1 y1 x3 y3 == False
    
    ------------------------------------------------------------------------------------
    
        if estUneLigne x1 y1 x2 y2
            then do
                let listex3y3 = recuperer2Elements updateListe
                let updateListe1 = retirerSes2Elements updateListe
                comptePourChacun listxy updateListe1 count
        else if estUneLigne x1 y1 x2 y2 == False
            then do 
                let listex3y3 = recuperer2Elements updateListe
                let updateListe1 = retirerSes2Elements updateListe
                comptePourChacun listxy updateListe1 count
        else if estUneLigne x1 y1 x2 y2 && estUneLigne x2 y2 x3 y3
            then do
                let listex3y3 = recuperer2Elements updateListe
                let updateListe1 = retirerSes2Elements updateListe
                if estUneLigneComplete x1 y1 x3 y3
                    then comptePourChacun listxy updateListe1 (count + 1)
                else comptePourChacun listxy updateListe1 count
    ------------------------------------------------------------------------------------------
    
        let listex3y3 = recuperer2Elements updateListe
        if estUneLigne x1 y1 x2 y2
            then do 
                --let listex3y3 = recuperer2Elements updateListe
                let x3 = head listex3y3
                let y3 = listex3y3 !! 1
                let updateListe1 = retirerSes2Elements updateListe
                
                if estUneLigne x2 y2 x3 y3
                    then do
                        if estUneLigneComplete x1 y1 x3 y3
                            then do
                                --putStrLn "count :-----------"
                                --print (count + 1) 
                                comptePourChacun listxy updateListe1 (count + 1)
                        else comptePourChacun listxy updateListe1 count
                else comptePourChacun listxy updateListe1 count
        else comptePourChacun listxy updateListe count

        --putStrLn$ "pour x1 y1 = " ++ show x1 ++ " : " ++ show y1
        --print updateListe
    
    
    {--if estUneLigne x1 y1 x2 y2
        then do 
            let listex3y3 = recuperer2Elements updateListe
            let x3 = head listex3y3
            let y3 = listex3y3 !! 1
            let updateListe1 = retirerSes2Elements updateListe
            
            if estUneLigne x2 y2 x3 y3
                then do
                    if estUneLigneComplete x1 y1 x3 y3
                        then 1 + comptePourChacun listxy updateListe1
                    else comptePourChacun listxy updateListe1
            else comptePourChacun listxy updateListe1
    else comptePourChacun listxy updateListe--}
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

ajouteValeurInListe :: [Int] -> Int -> [Int]
ajouteValeurInListe liste nb = nb : liste


main = do
    putStrLn "Hello World"
    let test = [1,2,3,4,5]
    
    let new = recuperer2Elements test
    print new 
    print test
    
    let update = retirerSes2Elements test
    print update
    
    let testAlign = [1,1,2,2,1,0,0,3,2,0,0,1,0,2]
    
    let testAlign1 = [3,3,2,2]
    
    putStrLn "-----------------------"
    
    --let count = compte testAlign
    --print count
    
    let listee = []
    
    --let new = ajouteValeurInListe listee 4
    --print new
    
    --let new1 = compte testAlign listee
    --print new1
    
    --let new2 = compte testAlign1 listee
    ---print new2
    
    --let n = retireEtRecupeX2Y2 [5,5] testAlign1
    --print n
    
    comptePourChacun [0,0] testAlign 0
    
    let nb = retireEtRecupeX3Y3 [0,0,1,1] [2,2]
    print nb
    

    
    
    
    
