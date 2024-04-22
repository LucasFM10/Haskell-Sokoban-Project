module GameLogic (
    isLevelWon,
    movePlayer,
    locatePlayer,
) where

import System.IO
import Types

nextPosition :: Position -> Dir -> Position
nextPosition (x, y) dir = case dir of
    Types.Left  -> (x, y - 1)
    Types.Right -> (x, y + 1)
    Types.Up    -> (x - 1, y)
    Types.Down  -> (x + 1, y)


-- Função para encontrar a posição do jogador no tabuleiro
locatePlayer :: Board -> Position
locatePlayer board = locatePlayerInBoard board 0

-- Função auxiliar para percorrer o tabuleiro linha por linha
locatePlayerInBoard :: Board -> Int -> Position
locatePlayerInBoard (row:rows) rowIndex =
    case locatePlayerInRow row 0 of
        Just colIndex -> (rowIndex, colIndex)  -- Encontrou o jogador na linha atual
        Nothing -> locatePlayerInBoard rows (rowIndex + 1)  -- Continua procurando nas próximas linhas

-- Função para encontrar o jogador em uma única linha
locatePlayerInRow :: [Tile] -> Int -> Maybe Int
locatePlayerInRow [] _ = Nothing  -- Chegou ao fim da linha sem encontrar o jogador
locatePlayerInRow (tile:rest) colIndex
    | isPlayer tile = Just colIndex  -- Encontrou o jogador
    | otherwise = locatePlayerInRow rest (colIndex + 1)  -- Continua na mesma linha

-- Função auxiliar para verificar se o Tile é um jogador ou jogador em um objetivo
isPlayer :: Tile -> Bool
isPlayer tile = tile == Player || tile == PlayerOnGoal


-- Função para mover o jogador em uma direção, assumindo que já conhecemos a sua posição
movePlayer :: Board -> Position -> Dir -> Board
movePlayer board pos dir =
    let newPos = nextPosition pos dir
        nextBoxPos = nextPosition newPos dir  -- Calcula a nova posição em que a caixa vai ficar (caso uma caixa seja empurrada)
    in case board !! fst newPos !! snd newPos of
        Wall -> board  -- Se a nova posição é uma parede, não move
        Box -> moveBoxIfPossible board pos newPos nextBoxPos
        BoxOnGoal -> moveBoxIfPossible board pos newPos nextBoxPos  -- Trata os possíveis movimentos da caixa
        Ground -> updateBoardWithNewPlayerPosition board pos newPos
        Goal -> updateBoardWithNewPlayerPosition board pos newPos -- Trata os possíveis movimentos caso o player não encontre obstruções

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
updateBoardWithNewPlayerPosition board oldPos newPos =
    let oldTile = if (board `at` oldPos) == PlayerOnGoal then Goal else Ground
        newTile = case (board `at` newPos) of
                    Goal -> PlayerOnGoal
                    _ -> Player
        updatedBoard = replaceTile board oldPos oldTile
    in replaceTile updatedBoard newPos newTile

-- Atualiza o tabuleiro movendo a caixa da posição antiga para a nova posição
updateBoardWithNewBoxPosition :: Board -> Position -> Position -> Board
updateBoardWithNewBoxPosition board oldBoxPos newBoxPos =
    let oldBoxTile = case (board `at` oldBoxPos) of
                        BoxOnGoal -> Goal
                        _    -> Ground
        newBoxTile = case (board `at` newBoxPos) of
                        Goal -> BoxOnGoal
                        _    -> Box
    in replaceTile (replaceTile board oldBoxPos oldBoxTile) newBoxPos newBoxTile

-- Adicionando uma função auxiliar para acessar o Board de forma segura
at :: Board -> Position -> Tile
at board (x, y) = (board !! x) !! y


-- Substitui um tile em uma posição específica usando Position
replaceTile :: Board -> Position -> Tile -> Board
replaceTile board (x, y) tile = 
    take x board ++ 
    [take y (board !! x) ++ [tile] ++ drop (y + 1) (board !! x)] ++ 
    drop (x + 1) board


isLevelWon :: Board -> Bool
isLevelWon board = not (any (elem Box) board)
