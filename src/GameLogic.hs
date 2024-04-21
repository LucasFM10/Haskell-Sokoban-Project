{-# LANGUAGE OverloadedStrings #-}

module GameLogic where

data Dir = L | R | U | D deriving (Show, Eq)
type Pos = (Int, Int)

data Player = Player { playerPos :: Pos } deriving (Show, Eq)
type Wall = [Pos]
type Box = [Pos] -- Adicionando o novo tipo para as caixas

move :: Dir -> Pos -> Wall -> Box -> (Pos, Box)
move dir (x, y) walls boxes =
    let newPos = case dir of
            L -> (x - 1, y)
            R -> (x + 1, y)
            U -> (x, y - 1)
            D -> (x, y + 1)
    in if newPos `elem` walls then ((x, y), boxes)
       else if newPos `elem` boxes
            then
                -- Verificar se a próxima posição atrás da caixa é um chão (não muro nem caixa)
                let nextBoxPos = case dir of
                        L -> (x - 2, y)
                        R -> (x + 2, y)
                        U -> (x, y - 2)
                        D -> (x, y + 2)
                in if not (nextBoxPos `elem` walls) && not (nextBoxPos `elem` boxes)
                   then (newPos, nextBoxPos : filter (/= newPos) boxes)
                   else ((x, y), boxes)
            else (newPos, boxes)
