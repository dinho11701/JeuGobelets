import Text.Parsec
import Text.Parsec.String
import Data.Either (rights)

data Size = T | S | M | B deriving Show
data Position = Position Int Int deriving Show
data Move = Drop Size Position | Onboard Position Position deriving Show

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parseSize :: Parser Size
parseSize = choice
    [ T <$ string "T"
    , S <$ string "S"
    , M <$ string "M"
    , B <$ string "B"
    ]

parsePosition :: Parser Position
parsePosition = do
    _ <- char '('
    x <- many digit
    _ <- char ','
    y <- many digit
    _ <- char ')'
    return $ Position (read x) (read y)

parseMove :: Parser Move
parseMove = try parseDrop <|> parseOnboard

parseDrop :: Parser Move
parseDrop = do
    _ <- string "drop"
    size <- lexeme parseSize
    pos <- lexeme parsePosition
    return (Drop size pos)

parseOnboard :: Parser Move
parseOnboard = do
    _ <- string "onboard"
    fromPos <- lexeme parsePosition
    _ <- string "to"
    toPos <- lexeme parsePosition
    return (Onboard fromPos toPos)
    
printParsedMove :: Move -> IO ()
printParsedMove (Drop size position) = do
    putStrLn "Drop :"
    putStrLn $ "Taille : " ++ show size
    putStrLn $ "Position en X : " ++ show (getX position)
    putStrLn $ "Position en Y : " ++ show (getY position)

printParsedMove (Onboard fromPos toPos) = do
    putStrLn "Onboard :"
    putStrLn $ "De la position : " ++ showPosition fromPos
    putStrLn $ "À la position : " ++ showPosition toPos

getX :: Position -> Int
getX (Position x _) = x

getY :: Position -> Int
getY (Position _ y) = y

showPosition :: Position -> String
showPosition (Position x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

main :: IO ()
main = do
    let moves = ["drop B (0, 1)", "onboard (0, 2) (2, 1)"]
    let parsedMoves = map (parse parseMove "") moves

    case sequence parsedMoves of
        Left err -> putStrLn $ "Erreur d'analyse : " ++ show err
        Right movesList -> do
            putStrLn "Mouvements parsés avec succès :"
            
            
            
    
            mapM_ printParsedMove movesList
