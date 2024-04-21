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
    walls :: Wall,
    boxes :: Box -- Adicionar o estado das caixas
} deriving (Show, Eq)

-- Aplicação principal da TUI
tuiApp :: App TuiState e ()
tuiApp =
    App {
        appDraw = drawTui,
        appChooseCursor = neverShowCursor,
        appHandleEvent = handleTuiEvent,
        appStartEvent = return,
        appAttrMap = const $ attrMap defAttr [("grass", bg black), ("player", bg blue), ("wall", bg white), ("box", bg yellow)]
    }
    
-- Função para construir o estado inicial do jogo
buildInitialState :: IO TuiState
buildInitialState = pure $ TuiState (9,9) (Player (4,4)) [(1,1), (1,2), (2,1)] [(3,3)] -- Exemplo: Uma caixa na posição (3,3)

-- Função para desenhar a interface do usuário
drawTui :: TuiState -> [Widget ()]
drawTui (TuiState _ (Player (px, py)) walls boxes) = [vBox rows]
  where
    rows = [hBox $ [cell x y | x <- [0..8]] | y <- [0..8]]
    cell x y = withAttr attr $ str symbol
      where
        (attr, symbol) = case (x, y) of
                          _ | (x, y) == (px, py) -> ("player", "😀")
                          _ | (x, y) `elem` walls -> ("wall", "🗿")
                          _ | (x, y) `elem` boxes -> ("box", "📦")
                          _ -> ("grass", "  ")


-- Função para manipular eventos de entrada
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey key [])) =
    let (newPlayerPos, newBoxes) = move dir (playerPos (player s)) (walls s) (boxes s)
        dir = case key of
            KLeft -> L
            KRight -> R
            KUp -> U
            KDown -> D
            _ -> undefined
    in case key of
        KChar 'q' -> halt s
        _ -> if dir `elem` [L, R, U, D]
             then continue $ s { player = (player s) { playerPos = newPlayerPos }, boxes = newBoxes }
             else continue s
handleTuiEvent s _ = continue s

-- Função para iniciar a TUI
tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState
