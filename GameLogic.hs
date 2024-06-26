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
    else board  -- Se a caixa estiver obstruída, o player não se move

-- Atualiza o tabuleiro movendo o jogador da posição antiga para a nova posição
updateBoardWithNewPlayerPosition :: Board -> Position -> Position -> Board
updateBoardWithNewPlayerPosition board currentPos newPos =
    replaceTile (replaceTile board currentPos currentTile) newPos newTile
    where
        currentTile = if board `at` currentPos == PlayerOnGoal then Goal else Ground
        newTile = if (board `at` newPos == Goal || board `at` newPos == BoxOnGoal) then PlayerOnGoal else Player

-- Atualiza o tabuleiro movendo a caixa da posição antiga para a nova posição
updateBoardWithNewBoxPosition :: Board -> Position -> Position -> Board
updateBoardWithNewBoxPosition board currentBoxPos newBoxPos =
    replaceTile board newBoxPos newBoxTile
    where
        newBoxTile = if board `at` newBoxPos == Goal then BoxOnGoal else Box


-- Adicionando uma função auxiliar para acessar o Board de forma segura
at :: Board -> Position -> Tile
at board (x, y) = (board !! x) !! y

-- Substitui um tile em uma posição específica do tabuleiro.
replaceTile :: Board -> Position -> Tile -> Board
replaceTile board (rowIndex, colIndex) newTile =
    -- Primeiro, pegamos todas as linhas antes da linha que queremos modificar
    let beforeRow = take rowIndex board
        -- Esta é a linha atual que vamos modificar
        currentRow = board !! rowIndex
        -- Agora, criamos a nova linha modificada:
        -- 1. Pegamos os tiles antes da posição desejada
        newRowBefore = take colIndex currentRow
        -- 2. Criamos a parte depois do tile que estamos substituindo
        newRowAfter = drop (colIndex + 1) currentRow
        -- 3. Combinamos o antes, o novo tile, e o depois para formar a nova linha
        newRow = newRowBefore ++ [newTile] ++ newRowAfter
        -- Por fim, pegamos todas as linhas depois da linha que modificamos
        afterRow = drop (rowIndex + 1) board
    -- Combinamos as partes para formar o novo tabuleiro
    in beforeRow ++ [newRow] ++ afterRow



isLevelWon :: Board -> Bool
isLevelWon board = not (any (elem Box) board)
