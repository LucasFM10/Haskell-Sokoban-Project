module GameLogic where

import System.IO

-- Definindo os tipos de tiles no jogo
data Tile = Wall | Ground | Box | Goal | Player | PlayerOnGoal | BoxOnGoal
  deriving (Show, Eq)

-- Definindo as direções possíveis para o movimento
data Dir = Up | Down | Left | Right
  deriving (Show, Eq)

-- Definindo o tipo para o tabuleiro, que será uma lista de listas de Tiles
type Board = [[Tile]]

-- Representa uma posição no tabuleiro
type Position = (Int, Int)

nextPosition :: Position -> Dir -> Position
nextPosition (x, y) dir = case dir of
    GameLogic.Left  -> (x, y - 1)
    GameLogic.Right -> (x, y + 1)
    Up              -> (x - 1, y)
    Down            -> (x + 1, y)


-- Função para encontrar a posição do jogador no tabuleiro
findPlayer :: Board -> Maybe Position
findPlayer board = findPlayerHelper board 0

-- Função auxiliar para percorrer o tabuleiro
findPlayerHelper :: Board -> Int -> Maybe Position
findPlayerHelper [] _ = Nothing  -- Não há mais linhas para verificar
findPlayerHelper (row:rows) rowIndex = 
    case findPlayerInRow row 0 of
        Just colIndex -> Just (rowIndex, colIndex)  -- Encontrou o jogador na linha atual
        Nothing -> findPlayerHelper rows (rowIndex + 1)  -- Continua procurando nas próximas linhas

-- Função para encontrar o jogador em uma única linha
findPlayerInRow :: [Tile] -> Int -> Maybe Int
findPlayerInRow [] _ = Nothing  -- Chegou ao fim da linha sem encontrar o jogador
findPlayerInRow (tile:rest) colIndex
    | tile == Player || tile == PlayerOnGoal = Just colIndex  -- Encontrou o jogador
    | otherwise = findPlayerInRow rest (colIndex + 1)  -- Continua na mesma linha


-- Função para mover o jogador em uma direção, assumindo que já conhecemos a sua posição
movePlayer :: Board -> Position -> Dir -> Board
movePlayer board pos dir =
    let newPos = nextPosition pos dir
        nextBoxPos = nextPosition newPos dir  -- Calcula a posição após a caixa
    in case board !! fst newPos !! snd newPos of
        Wall -> board  -- Se a nova posição é uma parede, não move
        Box -> moveBoxIfPossible board pos newPos nextBoxPos  -- Trata movimento de Box
        BoxOnGoal -> moveBoxIfPossible board pos newPos nextBoxPos  -- Trata movimento de BoxOnGoal
        _ -> updateBoardWithNewPlayerPosition board pos newPos  -- Para outras situações, move o jogador normalmente

isFreeSpace :: Board -> Position -> Bool
isFreeSpace board (x, y) =
    let tile = board !! x !! y
    in tile == Ground || tile == Goal

moveBoxIfPossible :: Board -> Position -> Position -> Position -> Board
moveBoxIfPossible board playerPos boxPos newBoxPos =
    if isFreeSpace board newBoxPos  -- Checa se o espaço após a caixa está livre
    then
        let boardAfterBoxMove = updateBoardWithNewBoxPosition board boxPos newBoxPos
        in updateBoardWithNewPlayerPosition boardAfterBoxMove playerPos boxPos
    else board  -- Se não estiver livre, não faz nada

moveBox :: Board -> Position -> Position -> Position -> Dir -> Board
moveBox board playerPos boxPos newBoxPos dir =
    let boardAfterBoxMove = updateBoardWithNewBoxPosition board boxPos newBoxPos
    in updateBoardWithNewPlayerPosition boardAfterBoxMove playerPos boxPos

-- Atualiza o tabuleiro movendo o jogador da posição antiga para a nova posição
updateBoardWithNewPlayerPosition :: Board -> Position -> Position -> Board
updateBoardWithNewPlayerPosition board (oldX, oldY) (newX, newY) =
    let oldTile = if board !! oldX !! oldY == PlayerOnGoal then Goal else Ground
        newTile = case board !! newX !! newY of
                    Goal -> PlayerOnGoal
                    _ -> Player
        updatedBoard = replaceTile board oldX oldY oldTile  -- Primeira atualização
    in replaceTile updatedBoard newX newY newTile  -- Segunda atualização


updateBoardWithNewBoxPosition :: Board -> Position -> Position -> Board
updateBoardWithNewBoxPosition board (boxX, boxY) (newBoxX, newBoxY) =
    let oldBoxTile = if board !! boxX !! boxY == BoxOnGoal then Goal else Ground
        newBoxTile = case board !! newBoxX !! newBoxY of
                        Goal -> BoxOnGoal
                        _    -> Box
    in replaceTile (replaceTile board boxX boxY oldBoxTile) newBoxX newBoxY newBoxTile




-- Substitui um tile em uma posição específica
replaceTile :: Board -> Int -> Int -> Tile -> Board
replaceTile board x y tile = 
    take x board ++ 
    [take y (board !! x) ++ [tile] ++ drop (y + 1) (board !! x)] ++ 
    drop (x + 1) board

-- Função que lê um arquivo de nível e converte para o tipo Board
readLevel :: FilePath -> IO Board
readLevel filePath = do
    contents <- readFile filePath
    let linesOfTiles = map (map charToTile) . lines $ contents
    return linesOfTiles

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

-- Configurações do terminal
setupTerminal :: IO ()
setupTerminal = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

-- Limpar a tela
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Imprime o tabuleiro na tela
printBoard :: Board -> IO ()
printBoard board = do
    clearScreen
    mapM_ (putStrLn . map tileToChar) board

tileToChar :: Tile -> Char
tileToChar Wall = '🧱'
tileToChar Ground = '⬛'
tileToChar Box = '🍉'
tileToChar Goal = '⭐'
tileToChar Player = '😎'
tileToChar PlayerOnGoal = '🤩'
tileToChar BoxOnGoal = '🌟'

isLevelWon :: Board -> Bool
isLevelWon board = not (any (elem Box) board)
