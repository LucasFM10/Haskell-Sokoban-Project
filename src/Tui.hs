{-# LANGUAGE OverloadedStrings #-}

module Tui where

import GameLogic
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Util
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

-- Tipo de dados para o estado do jogo na GUI
data TuiState = TuiState {
    boardSize :: Pos,
    player :: Player,
    walls :: Wall
} deriving (Show, Eq)

-- Aplicação principal da TUI
tuiApp :: App TuiState e ()
tuiApp =
    App {
        appDraw = drawTui,
        appChooseCursor = neverShowCursor,
        appHandleEvent = handleTuiEvent,
        appStartEvent = return,
        appAttrMap = const $ attrMap defAttr [("grass", bg black), ("player", bg blue), ("wall", bg white)]
    }

-- Função para construir o estado inicial do jogo
buildInitialState :: IO TuiState
buildInitialState = pure $ TuiState (9,9) (Player (4,4)) [(1,1), (1,2), (2,1)]

-- Função para desenhar a interface do usuário
drawTui :: TuiState -> [Widget ()]
drawTui (TuiState _ (Player (px, py)) walls) =
    [ vBox rows ]
    where
        rows = [ hBox $ [cell x y | x <- [0..8]] | y <- [0..8] ]
        cell x y
            | (x, y) == (px, py) = withAttr "player" $ str "  "
            | (x, y) `elem` walls = withAttr "wall" $ str "[]"
            | otherwise = withAttr "grass" $ str "  "

-- Função para manipular eventos de entrada
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey key [])) =
    case key of
        KLeft -> continue $ s { player = (player s) { playerPos = move L (playerPos (player s)) (walls s) } }
        KRight -> continue $ s { player = (player s) { playerPos = move R (playerPos (player s)) (walls s) } }
        KUp -> continue $ s { player = (player s) { playerPos = move U (playerPos (player s)) (walls s) } }
        KDown -> continue $ s { player = (player s) { playerPos = move D (playerPos (player s)) (walls s) } }
        KChar 'q' -> halt s
        _ -> continue s
handleTuiEvent s _ = continue s

-- Função para iniciar a TUI
tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState
