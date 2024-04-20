{-# LANGUAGE OverloadedStrings #-}

module GameLogic where

-- Tipos de dados para direções e posições
data Dir = L | R | U | D deriving (Show, Eq)
type Pos = (Int, Int)

-- Tipo de dados para o jogador e muros
data Player = Player { playerPos :: Pos } deriving (Show, Eq)
type Wall = [Pos]

-- Função para mover o jogador em uma direção, considerando muros
move :: Dir -> Pos -> Wall -> Pos
move dir (x, y) walls = let
    newPos = case dir of
        L -> (x - 1, y)
        R -> (x + 1, y)
        U -> (x, y - 1)
        D -> (x, y + 1)
    in if newPos `elem` walls then (x, y) else newPos
