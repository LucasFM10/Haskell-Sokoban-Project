import TerminalInterface (showMenu, setupTerminal)
import Types(Estado(..))
import LoadLevels

main :: IO ()
main = do
    setupTerminal Menu
    boards <- readLevels "levels.txt"  -- Supõe que este arquivo contém todos os níveis
    putStrLn "Todos os níveis foram carregados."
    showMenu "Bem vindo ao Sokoban! Escolha um nível para jogar: " boards