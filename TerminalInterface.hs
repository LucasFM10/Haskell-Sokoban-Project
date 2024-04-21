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
clearScreen = putStr "\ESC[2J\ESC[H"

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
    putStrLn "\nUtilize WASD para mover, M para menu, Q para sair, R para reiniciar."

-- Função auxiliar para converter um Tile para um caractere
tileToChar :: Tile -> Char
tileToChar Wall = '🧱'
tileToChar Ground = '⬛'
tileToChar Box = '🍉'
tileToChar Goal = '⭐'
tileToChar Player = '😎'
tileToChar PlayerOnGoal = '🤩'
tileToChar BoxOnGoal = '🌟'

-- Mostra o menu inicial e solicita ao usuário para escolher um nível
showMenu :: String -> [Board] -> IO ()
showMenu message boards = do
    putStrLn message
    input <- getLine
    if input == "quit" then do
        putStrLn "Encerrando o jogo. Até mais!"
        return ()  -- Encerra o programa
    else
        let parsedInput = reads input :: [(Int, String)]
        in case parsedInput of
            [(level, "")] ->
                if level >= 1 && level <= length boards then do
                    setupTerminal InGame
                    let selectedBoard = boards !! (level - 1)
                    gameLoop boards level selectedBoard
                else showMenu ("Nível inválido, por favor digite um número entre 1 e " ++ (show (length boards)) ++ ".") boards
            _ -> showMenu "Por favor, insira um número!" boards

-- Game loop que processa o jogo para um tabuleiro específico
gameLoop :: [Board] -> Int -> Board -> IO ()
gameLoop boards idx currentBoard = do
    let originalBoard = boards !! (idx - 1)
    printBoard currentBoard
    if isLevelWon currentBoard then do
        putStrLn "Você venceu! Aperte qualquer tecla para retornar ao menu."
        _ <- getChar
        setupTerminal Menu
        clearScreen
        showMenu "Bem vindo de volta! Qual nível você deseja enfrentar agora?" boards
    else do
        command <- getChar
        let newBoard = case command of
                'w' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Up
                's' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Down
                'a' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Types.Left
                'd' -> movePlayer currentBoard (fromJust $ findPlayer currentBoard) Types.Right
                'r' -> currentBoard  -- Reinicia o nível atual
                _   -> currentBoard
        case command of
            'm' -> do
                setupTerminal Menu
                clearScreen
                showMenu "Bem vindo de volta!" boards
            'q' -> return ()
            'r' -> gameLoop boards idx originalBoard  -- Reinicia o nível
            _   -> gameLoop boards idx newBoard

