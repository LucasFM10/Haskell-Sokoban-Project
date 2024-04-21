module GameLogic (
    isLevelWon,
    movePlayer,
    findPlayer,
) where

import System.IO
import Types

nextPosition :: Position -> Dir -> Position
nextPosition (x, y) dir = case dir of
    Types.Left  -> (x, y - 1)
    Types.Right -> (x, y + 1)
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

isLevelWon :: Board -> Bool
isLevelWon board = not (any (elem Box) board)
