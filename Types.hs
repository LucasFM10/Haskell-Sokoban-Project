module Types where

-- Definindo os tipos de tiles no jogo
data Tile = Wall | Ground | Box | Goal | Player | PlayerOnGoal | BoxOnGoal
  deriving (Show, Eq)

-- Definindo o tipo para o tabuleiro, que será uma lista de listas de Tiles
type Board = [[Tile]]

-- Definindo as direções possíveis para o movimento
data Dir = Up | Down | Left | Right
  deriving (Show, Eq)

-- Representa uma posição no tabuleiro
type Position = (Int, Int)

-- Novo tipo para estados do jogo
data Estado = InGame | Menu
  deriving (Show, Eq)