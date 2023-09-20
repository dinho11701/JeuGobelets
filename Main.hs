-- Définition d'un type de données pour la structure
data Joueur = Joueur { alignement1 :: Int
                     , alignement2 :: Int
                     , listeVariables :: [Int]
                     , score :: Int
                     } deriving (Show)
                     

-- Fonction pour calculer le score comme la somme des variables
calculerScore :: Joueur -> Joueur
calculerScore joueur = joueur { score = sum (listeVariables joueur) }


-- Exemple d'utilisation
main :: IO ()
main = do
    -- Création d'une instance de Joueur avec le score calculé
    let joueur1 = calculerScore (Joueur { alignement1 = 10
                                        , alignement2 = 20
                                        , listeVariables = [10, 20]
                                        , score = 0
                                        })

    -- Affichage des attributs de l'objet
    putStrLn ("Variable 1 : " ++ show (alignement1 joueur1))
    putStrLn ("Variable 2 : " ++ show (alignement2 joueur1))
    putStrLn ("Liste de variables : " ++ show (listeVariables joueur1))
    putStrLn ("Score du joueur : " ++ show (score joueur1))
