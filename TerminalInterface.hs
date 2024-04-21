module TerminalInterface (
    printBoard,
    setupTerminal,
    clearScreen,
    showMenu,
) where

import System.IO (hSetEcho, hSetBuffering, BufferMode(..), stdin, stdout)
import Data.Maybe (fromJust)
import GameLogic
import Types

-- Limpar a tela
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Configurações do terminal baseadas no estado
setupTerminal :: Estado -> IO ()
setupTerminal estado = do
    case estado of
        InGame -> do
            hSetEcho stdin False
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
        Menu -> do
            hSetEcho stdin True
            hSetBuffering stdin LineBuffering
            hSetBuffering stdout LineBuffering
-- Imprime o tabuleiro na tela
printBoard :: Board -> IO ()
printBoard board = do
    clearScreen
    mapM_ (putStrLn . map tileToChar) board

-- Função auxiliar para converter um Tile para um caractere
tileToChar :: Tile -> Char
tileToChar Wall = '🧱'
tileToChar Ground = '⬛'
tileToChar Box = '🍉'
tileToChar Goal = '⭐'
tileToChar Player = '😎'
tileToChar PlayerOnGoal = '🤩'
tileToChar BoxOnGoal = '🌟'

-- Divide o conteúdo do arquivo em níveis separados por linhas vazias
splitLevels :: [String] -> [[String]]
splitLevels lines = filter (not . null) $ foldr splitFunc [[]] lines
  where
    splitFunc line (acc:accs)
        | null line = [] : acc : accs  -- Inicia um novo nível quando encontra uma linha vazia
        | otherwise = (line : acc) : accs  -- Adiciona a linha ao nível atual

-- Mostra o menu inicial e solicita ao usuário para escolher um nível
showMenu :: String -> [Board] -> IO ()
showMenu message boards = do
    putStrLn message
    input <- getLine
    let parsedInput = reads input :: [(Int, String)]
    case parsedInput of
        [(level, "")] ->
            if level >= 1 && level <= length boards then do
                setupTerminal InGame
                let selectedBoard = boards !! (level - 1)
                gameLoop boards selectedBoard  -- Passa o tabuleiro escolhido e a lista completa de tabuleiros
            else showMenu ("Número inválido, por favor digite um número entre 1 e " ++ show (length boards)) boards
        _ -> showMenu "Por favor, insira um número válido." boards

-- Game loop que processa o jogo para um tabuleiro específico
gameLoop :: [Board] -> Board -> IO ()
gameLoop boards currentBoard = do
    printBoard currentBoard
    if isLevelWon currentBoard then do
        putStrLn "Você venceu! Aperte qualquer tecla para retornar ao menu."
        _ <- getChar
        showMenu "Bem vindo de volta! Qual nível você deseja enfrentar agora?" boards
    else do
        command <- getChar
        let newBoard = case command of
                'w' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Up
                's' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Down
                'a' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Types.Left
                'd' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Types.Right
                _   -> currentBoard
        if command `elem` ['w', 's', 'a', 'd']
            then gameLoop boards newBoard
            else case command of
                'm' -> showMenu "Bem vindo de volta! Qual nível você deseja enfrentar agora?" boards
                'q' -> return ()
                _   -> gameLoop boards currentBoard