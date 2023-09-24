import Move

-- Définition d'un type de données pour la structure
data Joueur = Joueur { alignement1 :: Int
                     , alignement2 :: Int
                     , listeVariables :: [Int] ---liste des Alignements
                     , scoreTotal :: Int --score du joueur
                     } deriving (Show)
                     

-- Fonction pour calculer le score comme la somme des alignements d'un joueur
score :: Joueur -> Joueur
score joueur = joueur { scoreTotal = sum (listeVariables joueur) }


data PieceUser = Zero0 | Zero1 | Zero2 | Zero3 deriving (Eq,Show)

data PieceComputer = X0 | X1 | X2 | X3 deriving (Eq,Show)

transformerPieceUserEnInt :: PieceUser -> Int
transformerPieceUserEnInt intUser = case intUser of
    Zero0 -> 00
    Zero1 -> 01
    Zero2 -> 02
    Zero3 -> 03


-- Exemple d'utilisation
main :: IO ()
main = do
    -- Création d'une instance de Joueur avec le score calculé
    let joueur1 = score (Joueur { alignement1 = 10
                                        , alignement2 = 20
                                        , listeVariables = [10, 20]
                                        , scoreTotal = 0
                                        })

    let jeu = [ ["03", "", "__", "XX"]
                  , ["02", "X3", "__", "__"]
                  , ["01", "__", "__", "__"]
                  , ["AllahOuakbar", "__", "__", "__"] ]

    --putStrLn ("Est-ce que " ++ show (transformerPieceUserEnInt Zero0 `elem` tableauJeu)
    

    -- Affichage des attributs de l'objet
    putStrLn ("Variable 1 : " ++ show (alignement1 joueur1))
    putStrLn ("Variable 2 : " ++ show (alignement2 joueur1))
    putStrLn ("Liste de variables : " ++ show (listeVariables joueur1))
    putStrLn ("Score du joueur : " ++ show (scoreTotal joueur1))

    let listeMovRest1 = [listeBUser,listeMUser,listeSUser]
    let listeMovRest2 = [listeBComp,listeMComp,listeSComp]

    let new1 = formaterListeMov listeMovRest1
    let new2 = formaterListeMov listeMovRest2

    print jeu
    putStrLn (new1 ++ " || " ++ new2)
