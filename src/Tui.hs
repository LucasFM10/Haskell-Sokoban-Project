{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Util
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

-- O tipo 'Dir' representa as quatro direções de movimento.
data Dir = L | R | U | D deriving (Show, Eq)

-- 'Pos' representa uma posição no espaço bidimensional.
type Pos = (Int, Int)

-- 'Player' representa o jogador e sua posição atual.
data Player = Player { playerPos :: Pos } deriving (Show, Eq)

-- 'TuiState' inclui agora o 'Player'.
data TuiState = TuiState {
    boardSize :: Pos,
    player :: Player
} deriving (Show, Eq)

-- Função para mover o jogador em uma direção.
move :: Dir -> Pos -> Pos
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)

tuiApp :: App TuiState e ()
tuiApp =
  App {
    appDraw = drawTui,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleTuiEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap defAttr [("grass", bg ), ("player", bg blue)]
  }

-- Função para construir o estado inicial do jogo.
buildInitialState :: IO TuiState
buildInitialState = pure $ TuiState (9,9) (Player (4,4))

drawTui :: TuiState -> [Widget ()]
drawTui (TuiState _ (Player (px, py))) =
  [ vBox rows ]
  where
    rows = [ hBox $ [cell x y | x <- [0..8]] | y <- [0..8] ]
    cell x y
      | x == px && y == py = withAttr "player" $ str "  "
      | otherwise = withAttr "grass" $ str "  "

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey key [])) =
  case key of
    KLeft -> continue $ s { player = (player s) { playerPos = move L (playerPos (player s)) } }
    KRight -> continue $ s { player = (player s) { playerPos = move R (playerPos (player s)) } }
    KUp -> continue $ s { player = (player s) { playerPos = move U (playerPos (player s)) } }
    KDown -> continue $ s { player = (player s) { playerPos = move D (playerPos (player s)) } }
    KChar 'q' -> halt s
    _ -> continue s
handleTuiEvent s _ = continue s

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState
