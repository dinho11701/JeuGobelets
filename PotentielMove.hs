module Move where

import Text.Parsec
import Text.Parsec.String

data Size = T | S | M | B deriving Show
data Position = Position Int Int deriving Show
data Move = Drop Size Position | Onboard Position Position deriving Show

-- Analyseur pour les tailles
sizeParser :: Parser Size
sizeParser = choice
    [ T <$ char 'T'
    , S <$ char 'S'
    , M <$ char 'M'
    , B <$ char 'B'
    ]

-- Analyseur pour les positions
positionParser :: Parser Position
positionParser = do
    char '('
    x <- digit
    char ','
    spaces
    y <- digit
    char ')'
    return $ Position (read [x]) (read [y])

-- Analyseur pour les mouvements "onboard"
{--onboardParser :: Parser Move
onboardParser = do
    string "onboard"
    spaces
    position1 <- positionParser
    spaces
    string "to"
    spaces
    position2 <- positionParser
    return (Onboard position1 position2)
--}


--onboard((0, 2), (2, 1))



--drop(B, (0, 1))
dropParser :: Parser Move
dropParser = do
    string "drop"
    char '('
    size <- sizeParser
    char ','
    optional spaces  -- Rend l'espace optionnel
    position <- positionParser
    char ')'
    return (Drop size position)



onboardParser :: Parser Move
onboardParser = do
    string "onboard"
    char '('
    position1 <- positionParser
    char ','
    optional spaces  -- Rend l'espace optionnel
    position2 <- positionParser
    char ')'
    return (Onboard position1 position2)




-- Analyseur pour les mouvements
parseMoves :: Parser Move
parseMoves = try onboardParser <|> dropParser


import Move
import Text.Parsec

main :: IO ()
main = do
  --let moves = ["onboard((0,2), (2,1))"]
  let moves = ["drop(B, (0, 1))"] 

  case runParser parseMoves () "input" (unlines moves) of
    Right result -> print result
    Left err -> print err

