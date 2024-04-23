-- Trabalho dos alunos:
-- LUCAS FARIAS DE MEDEIROS 20220054884
-- JOAO VICTOR MENDONCA DE CARVALHO 20200096706 

import TerminalInterface (showMenu, setupTerminal)
import Types(Estado(..))
import LoadLevels(readLevels)

main :: IO ()
main = do
    setupTerminal Menu
    boards <- readLevels "levels.txt"
    putStrLn "Todos os níveis foram carregados."
    showMenu ("Bem vindo ao Sokoban!\nDesenvolvido por Lucas Farias e João Victor para a cadeira de programação funcional!\nEscolha um nível para jogar digitando um número entre 1 e " ++ show (length boards) ++ " ou escreva 'quit' para encerrar o jogo.") boards

