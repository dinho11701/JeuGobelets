module Piece where

-- Définition d'un type de données pour la structure
data Piece = Piece { taille :: String
                     , nomPiece :: String --exemple x3,x2,x1
                     } deriving (Show)


