import TerminalInterface (showMenu, setupTerminal)
import Types(Estado(..))
import LoadLevels

main :: IO ()
main = do
    setupTerminal Menu
    boards <- readLevels "levels.txt"  -- Supõe que este arquivo contém todos os níveis
    putStrLn "Todos os níveis foram carregados."
    showMenu ("Bem vindo ao Sokoban!\nDesenvolvido por Lucas Farias e João Victor para a cadeira de programação funcional!\nEscolha um nível para jogar digitando um número entre 1 e " ++ show (length boards) ++ " ou escreva 'quit' para encerrar o jogo.") boards