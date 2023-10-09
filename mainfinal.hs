import Joueur
import System.IO
import Liste 

--ca me eprmet de renvoyer 2 listes (le jeu,la listeCoorespondante modifie du joueur)
data ListeModifie = ListeModifie [[String]] [String]


jeu = [ ["O3", "__", "__", "X3"]
                  , ["O2", "X3", "__", "__"]
                  , ["O1", "__", "__", "__"]
                  , ["O0", "__", "__", "__"] ]


diffTaillePiece = ["B", "M", "S", "T"]


-- recree la liste dependamment de quelle piece a changer grace a sa taille
updateNewListePieceJoueurs :: String -> [String] -> [[String]]
updateNewListePieceJoueurs taille listeModifie
    | taille == "B" = [listeModifie,listeMUser,listeSUser,listeTUser]
    | taille == "M" = [listeBUser,listeModifie,listeSUser,listeTUser]
    | taille == "S" = [listeBUser,listeMUser,listeModifie,listeTUser]
    | taille == "T" = [listeBUser,listeMUser,listeSUser,listeModifie]



-- Fonction pour lire une action depuis l'utilisateur
lectureActionPlayer :: IO String
lectureActionPlayer = do
  putStr "> "  -- Affiche un prompt pour l'utilisateur
  hFlush stdout  -- Vide le tampon de sortie pour s'assurer que le prompt s'affiche immédiatement
  getLine  -- Lit une ligne d'entrée depuis la console



-- Fonction pour afficher un message de bienvenue interactif
messageBienvenue :: IO ()
messageBienvenue = do
  putStrLn "Bienvenue dans le jeu Gobblet !"
  putStrLn "Dans ce jeu, vous pouvez entrer des actions pour interagir."
  putStrLn "Par exemple, vous pouvez entrer 'drop(M, (0,1))' pour jouer une piece à la position (0,1)."
  putStrLn "Entrez 'quitter' pour quitter le jeu."




--meilleur version , plus clean
modifierCase2D :: [[a]] -> a -> Int -> Int -> [[a]]
modifierCase2D tableau nouvelleValeur ligne colonne
  | ligne < 0 || ligne >= length tableau || colonne < 0 || colonne >= length (tableau !! 0) = tableau
  | otherwise =
    let (avant, apres) = splitAt colonne (tableau !! ligne)
    in take ligne tableau ++ [avant ++ [nouvelleValeur] ++ tail apres] ++ drop (ligne + 1) tableau


drop1 :: String -> Int -> Int -> ListeModifie
drop1 taille x y
  | taille `elem` diffTaillePiece =
    let (pieces, diffPieceJoueur) = case taille of
          "B" -> (listeBUser, listeBUser)
          "M" -> (listeMUser, listeMUser)
          "S" -> (listeSUser, listeSUser)
          "T" -> (listeTUser, listeTUser)
          _   -> ([], ["pas bon"])
        pieceAJouer = retirerTeteListe pieces
        newListePiece = prendResteListe diffPieceJoueur
        jeu1 = modifierCase2D jeu pieceAJouer x y
    in ListeModifie jeu1 newListePiece
  | otherwise = ListeModifie [[]] ["pas bon"]


onboard :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
onboard jeuDepart y1 x1 y2 x2
  | x1 < 0 || x1 >= length jeuDepart || y1 < 0 || y1 >= length (jeuDepart !! 0) || x2 < 0 || x2 >= length jeuDepart || y2 < 0 || y2 >= length (jeuDepart !! 0) = jeuDepart
  | otherwise =
    let pieceADeplacer = jeuDepart !! x1 !! y1
        jeuTemporaire = modifierCase2D jeuDepart pieceADeplacer x2 y2
        jeuFinal = modifierCase2D jeuTemporaire "-" x1 y1
    in jeuFinal
    


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
        


peutJouerCase :: [Int] -> Int -> Int -> Int -> Bool
peutJouerCase listePosCasesVide i x y = do
    if i < length listePosCasesVide && (i + 1) < length listePosCasesVide
        then do 
            let x1 = listePosCasesVide !! i 
                y1 = listePosCasesVide !! (i + 1)
            if x1 == x && y1 == y
                then True
            else peutJouerCase listePosCasesVide (i + 2) x y
    else False
                    
    
--verifAlign3PiecesAutreJr listeCoordAdvers 


chercheLeTriplet :: [Int] -> [Int] -> [Int]
chercheLeTriplet listexy listeComplete = do
    let x1 = listexy !! 0
        y1 = listexy !! 1
        x2y2 = retourneSonX2Y2 listexy listeComplete 0
        
    if length x2y2 == 0
        then []
    else do
        let x2 = x2y2 !! 0
            y2 = x2y2 !! 1
            couple = [x1,y1,x2,y2]
            triplet = creerTriplet couple listeComplete 0
            
        if length triplet == 6
            then triplet
        else []

joueSurUneCaseAdverse :: [[String]] -> String -> Int -> Int -> [[String]]
joueSurUneCaseAdverse jeu piece x y = modifierCase2D jeu piece x y
    
    
    
main :: IO ()
main = do
    --putStrLn "Hello gogo"
    
    messageBienvenue
  
    depart <- lectureActionPlayer
    putStrLn $ "Vous avez entré : " ++ depart
    
    let listeMovRest1 = [listeBUser,listeMUser,listeSUser,listeTUser]
    let listeMovRest2 = [listeBComp,listeMComp,listeSComp,listeTComp]

    let new1 = formaterListeMov listeMovRest1
    let new2 = formaterListeMov listeMovRest2

    --print jeu
    --putStrLn (new1 ++ " || " ++ new2)
    
    let taille = "B"
    
    let (ListeModifie jeuUpdate liste) = drop1 taille 0 0
    print jeuUpdate
    print liste
    
    let listePieceUser = updateNewListePieceJoueurs taille liste
    
    let new11 = formaterListeMov listePieceUser 
    putStrLn (new11 ++ " || " ++ new2)
    
    ----determiner si un joueur a 4 pieces alignées, si oui fin du jeu, sinon continue
    
    
    putStrLn "Hello World"
    
    
    let test1 = [0,0,1,0,2,0,0,1,1,1,1,2,2,2,0,3,3,3]
    
    let maListe = [1,1,2,3,4,5,2,3]

    let pos1 = existeAlign4 [0,0] test1
    print pos1
    
    let re = estLeJoueurGagnant test1 0
    print re
    
    let (ListeCoord posPieceJr posPieceOrdi) = creerListeCoordPieces jeu [] [] 0 0
    print posPieceJr
    print posPieceOrdi

    let val = estUneCaseVide (jeu !! 0 !! 0)
    print val 
    
    
    ---s'il a droit de jouer ce xy qui est une case vide, fais le drop etc
    
    --si la position de la piece qu'il veut jouer est une case vide , gooo jooue ton drop 
    --Il y a des cases vides?
    
    ---si taille liste /= 0 , il ya des cases vides
    let liste = donneListCasesVide jeu [] 0 0 
    print liste
    
    --est-ce que son mov est valide alors pour une case vide?
    --si true , il peut drop ,sinon il doit recommencer peut-etre en choisissant un autre emplacement(plus tard)
    let rep = peutJouerCase liste 0 0 3
    print rep 
    
    
    ---En premier : verif d'abord que l'autre jr a pas un align de 3 pieces 
    --verifAlign3PiecesAutreJr ListeCoordJrAdvers 
    -- si ListeCoordJrAdvers vide, retourne liste vide 
    --s'il en a 1, retourne la list position de ces 3 pieces et je dois mettre ma piece a la position d'une de ces cases ds le jeu 
    --sinon retourne liste vide 
    
    let triplet = chercheLeTriplet [0,0] [0,0,3,0,0,1,1,1,0,2,0,3]
    print triplet
    
    let rep1 = peutJouerCase triplet 0 0 2
    print rep1
    
    let newJeu = joueSurUneCaseAdverse jeu "X3" 0 0
    print newJeu
    
    
    printList2DDictio liste2DAvecDictionnaire
    
    -- Exemple d'utilisation : ajouter la clé "B" avec la valeur 123 à la case (1, 2)
    let updatedBigList2D = addToDictionaryIn2DList 1 2 "B" "X3" liste2DAvecDictionnaire
    --printList2D updatedBigList2D
    
    let updatedBigList2D1 = addToDictionaryIn2DList 3 3 "S" "X3" updatedBigList2D
    printList2DDictio updatedBigList2D1
    
    let updatedBigList2D2 = addToDictionaryIn2DList 3 3 "B" "X3" updatedBigList2D1
    printList2DDictio updatedBigList2D2
    
    --addToDictionaryIn2DList 1 2 "B" "X3" liste2DAvecDictionnaire
    
    ---quand je modifie une case du jr adversaire, je dois modif la pile des pieces en ajoutant la piece avec sa taille
    ---on se rappelle, je dois faire une liste de liste contenant pour chacune un dictionnaire avec pour chaque case, sa cle qui est
    ---la taille et SA VALEUR QUI EST LA PIECE 
    
    
    --if length triplet == 0
        ----joue les cases vides
    --else do
        ------- joue sur une de ces cases triplet
        --let rep1 = peutJouerCase triplet 0 0 3
        -- drop1 sur une de ces cases du triplet
    
    ------------------------------------------------------------------------------------
    
    
    
    
    
    
    
