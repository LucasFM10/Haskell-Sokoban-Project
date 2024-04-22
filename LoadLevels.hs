module LoadLevels (
    readLevels
) where

import Types

-- Função auxiliar para converter um caractere para um Tile
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile ' ' = Ground
charToTile 'o' = Box
charToTile '.' = Goal
charToTile '@' = Player
charToTile '+' = PlayerOnGoal
charToTile '*' = BoxOnGoal
charToTile _   = error "Invalid tile character"

-- Função que lê um arquivo e divide em níveis baseados em linhas vazias
readLevels :: FilePath -> IO [Board]
readLevels filePath = do
    contents <- readFile filePath
    let levelStrings = splitLevels (lines contents)
    return $ map parseLevel levelStrings

-- Divide o conteúdo do arquivo em níveis separados por linhas vazias
splitLevels :: [String] -> [[String]]
splitLevels allLines = filter (not . null) (foldr addToLevel [[]] allLines)
  where
    addToLevel currentLine (currentLevel:restOfLevels)
        | null currentLine = [] : currentLevel : restOfLevels  -- Começa um novo nível se a linha atual é vazia
        | otherwise = (currentLine : currentLevel) : restOfLevels  -- Adiciona a linha atual ao nível atual


-- Converte um bloco de texto em um Board
parseLevel :: [String] -> Board
parseLevel = map (map charToTile)
