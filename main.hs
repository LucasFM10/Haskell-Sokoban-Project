import GameLogic
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))
import Data.Maybe (fromJust)

main :: IO ()
main = do
    setupTerminal
    putStrLn "Digite o nome do arquivo do nível:"
    filename <- getLine
    board <- readLevel ("./levels/" ++ filename ++ ".txt")
    gameLoop board

gameLoop :: Board -> IO ()
gameLoop board = do
    printBoard board
    if isLevelWon board
    then putStrLn "Parabéns, você completou o nível!" >> return ()
    else do
        command <- getChar
        case command of
            'w' -> gameLoop $ movePlayer board (fromJust $ findPlayer board) Up
            's' -> gameLoop $ movePlayer board (fromJust $ findPlayer board) Down
            'a' -> gameLoop $ movePlayer board (fromJust $ findPlayer board) GameLogic.Left
            'd' -> gameLoop $ movePlayer board (fromJust $ findPlayer board) GameLogic.Right
            'q' -> return ()  -- Permitir ao usuário sair do jogo
            _   -> gameLoop board  -- Se pressionar uma tecla não mapeada, não faz nada e recomeça o loop

