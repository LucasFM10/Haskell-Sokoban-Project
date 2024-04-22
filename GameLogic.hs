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
    Types.Up    -> (x - 1, y)
    Types.Down  -> (x + 1, y)


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
