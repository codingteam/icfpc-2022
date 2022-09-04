module Editor (main) where

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust)
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.Gloss.Juicy as G
import System.IO.Unsafe (unsafePerformIO)
import Types

data EditorState = EditorState
  { esBackground :: G.Picture,
    esBlocks :: HMS.HashMap BlockId Shape,
    esZoom :: Float,
    esTranslate :: (Float, Float),
    esSelected :: Maybe BlockId,
    esDragStart :: Maybe (Float, Float)
  }

main :: String -> IO ()
main path =
  G.play
    (G.InWindow "Editor" (200, 200) (10, 10))
    G.white
    30
    (initialEditorState path)
    draw
    handleEvent
    handleTime

initialEditorState :: String -> EditorState
initialEditorState path =
  EditorState
    { esBackground = fromJust $ unsafePerformIO $ G.loadJuicyPNG path,
      esBlocks = HMS.singleton (createBlockId 0) (Rectangle 0 0 400 400),
      esZoom = 0,
      esTranslate = (0, 0),
      esSelected = Nothing,
      esDragStart = Nothing
    }

draw :: EditorState -> G.Picture
draw state = drawBlocks
  where
    zoom = 2 ** esZoom state
    drawBlocks =
      uncurry G.translate (esTranslate state)
        . G.scale zoom zoom
        . G.pictures
        $ esBackground state :
        ( map drawBlock
            . HMS.toList
            . esBlocks
            $ state
        )
    drawBlock (blockId, shape) =
      G.color G.black
        . G.translate (fromIntegral $ rX shape) (fromIntegral $ rY shape)
        $ G.rectangleWire (fromIntegral $ rWidth shape) (fromIntegral $ rHeight shape)

handleEvent :: G.Event -> EditorState -> EditorState
handleEvent (G.EventKey (G.MouseButton G.WheelDown) G.Down _ _) state =
  state {esZoom = esZoom state - 0.1}
handleEvent (G.EventKey (G.MouseButton G.WheelUp) G.Down _ _) state =
  state {esZoom = esZoom state + 0.1}
handleEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (x, y)) state =
  state {esDragStart = Just (x, y)}
handleEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up _ _) state =
  state {esDragStart = Nothing}
handleEvent (G.EventMotion (x, y)) state@EditorState {esDragStart = Just (x0, y0)} =
  state
    { esDragStart = Just (x, y),
      esTranslate = bimap ((x - x0) +) ((y - y0) +) (esTranslate state)
    }
handleEvent _ state = state

handleTime :: Float -> EditorState -> EditorState
handleTime _ = id
