import Joueur
import System.IO


jeu = [ ["03", "", "__", "XX"]
                  , ["02", "X3", "__", "__"]
                  , ["01", "__", "__", "__"]
                  , ["AllahOuakbar", "__", "__", "__"] ]


--ca me eprmet de renvoyer 2 listes (le jeu,la listeCoorespondante modifie du joueur)
data ListeModifie = ListeModifie [[String]] [String]

diffTaillePiece = ["B", "M", "S", "T"]

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
    
    
    
    
    
    
    
