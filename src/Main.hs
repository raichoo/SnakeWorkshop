{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (Left, Right, head, tail)

import qualified Graphics.UI.SDL as SDL

import qualified Foreign.C.String      as Foreign
import qualified Foreign.Ptr           as Foreign
import qualified Foreign.C.Types       as Foreign
import qualified Foreign.Storable      as Foreign
import qualified Foreign.Marshal.Alloc as Foreign

import Data.IORef
import Control.Monad (forM_, when, unless, void)

import Snake

type EventHandler = Foreign.Ptr () -> Foreign.Ptr SDL.Event -> IO Foreign.CInt

foreign import ccall "wrapper"
  mkEventFilter :: EventHandler -> IO (Foreign.FunPtr EventHandler)

data GameTextures = GameTextures { backgroundTexture :: SDL.Texture
                                 , bodyTexture       :: SDL.Texture
                                 , headTexture       :: SDL.Texture
                                 , yellowItemTexture :: SDL.Texture
                                 , redItemTexture    :: SDL.Texture
                                 , greenItemTexture  :: SDL.Texture
                                 , blockTexture      :: SDL.Texture
                                 }

gameOver :: GameState -> IO Bool
gameOver state =
  case phase state of
       Won  -> return True
       Lost -> return True
       _    -> return False

changeDirection :: Direction -> GameState -> GameState
changeDirection dir state@(GameState {..}) =
  state { direction = dir }

eventHandler :: IORef GameState -> EventHandler
eventHandler state _ ev = do
  event <- Foreign.peek ev
  case event of
       SDL.KeyboardEvent {..} ->
         when (eventType == SDL.eventTypeKeyDown) $
           keypressed $ SDL.keysymScancode keyboardEventKeysym
       _                      -> return ()
  return 1
  where
    keypressed :: SDL.Scancode -> IO ()
    keypressed scancode
      | scancode == SDL.scancodeUp    =
          modifyIORef state (changeDirection Up)
      | scancode == SDL.scancodeDown  =
          modifyIORef state (changeDirection Down)
      | scancode == SDL.scancodeLeft  =
          modifyIORef state (changeDirection Left)
      | scancode == SDL.scancodeRight =
          modifyIORef state (changeDirection Right)
      | otherwise = return ()

render :: SDL.Renderer -> GameTextures -> GameState -> IO ()
render renderer (GameTextures {..}) state = do
  SDL.renderClear renderer
  SDL.renderCopy renderer backgroundTexture Foreign.nullPtr Foreign.nullPtr

  renderSnake (snake state)
  renderItems (items state)
  renderBlocks (blocks state)

  SDL.renderPresent renderer
  where
    renderSnake :: [Coord] -> IO ()
    renderSnake (head:tail) = do
      renderSprite headTexture head
      forM_ tail (renderSprite bodyTexture)
    renderSnake _           = return ()

    renderItems :: [Item] -> IO ()
    renderItems = mapM_ renderItem

    renderBlocks :: [Coord] -> IO ()
    renderBlocks = mapM_ (renderSprite blockTexture)

    renderItem :: Item -> IO ()
    renderItem (itemtype, coord) =
      renderSprite texture coord
      where
        texture :: SDL.Texture
        texture = case itemtype of
                       Red    -> redItemTexture
                       Green  -> greenItemTexture
                       Yellow -> yellowItemTexture

    renderSprite :: SDL.Texture -> Coord -> IO ()
    renderSprite texture sprite =
      let x = fromIntegral $ fst sprite
          y = fromIntegral $ snd sprite
          w = fromIntegral spriteWidth
          h = fromIntegral spriteHeight
          r = SDL.Rect x y w h
       in Foreign.alloca $ \dst -> do
            Foreign.poke dst r
            void $ SDL.renderCopy renderer texture Foreign.nullPtr dst

loadTexture :: SDL.Renderer -> String -> IO SDL.Texture
loadTexture renderer file = do
  image   <- Foreign.newCString file
  surface <- SDL.loadBMP image
  texture <- SDL.createTextureFromSurface renderer surface

  SDL.freeSurface surface
  Foreign.free image

  return texture

renderBackground :: SDL.Renderer -> SDL.Texture -> IO ()
renderBackground renderer texture =
  void $ SDL.renderCopy renderer texture Foreign.nullPtr Foreign.nullPtr

loadTextures :: SDL.Renderer -> IO GameTextures
loadTextures renderer = do
  background <- loadTexture renderer "images/background.bmp"
  head       <- loadTexture renderer "images/head.bmp"
  body       <- loadTexture renderer "images/body.bmp"
  yellow     <- loadTexture renderer "images/yellow.bmp"
  red        <- loadTexture renderer "images/red.bmp"
  green      <- loadTexture renderer "images/green.bmp"
  block      <- loadTexture renderer "images/block.bmp"
  return $ GameTextures background body head yellow red green block

freeTextures :: GameTextures -> IO ()
freeTextures (GameTextures {..}) = do
  SDL.destroyTexture backgroundTexture
  SDL.destroyTexture bodyTexture
  SDL.destroyTexture headTexture
  SDL.destroyTexture yellowItemTexture
  SDL.destroyTexture redItemTexture
  SDL.destroyTexture greenItemTexture
  SDL.destroyTexture blockTexture

withSDL :: (SDL.Renderer -> IO ()) -> IO ()
withSDL act = do
  let w = fromIntegral screenWidth
      h = fromIntegral screenHeight

  name     <- Foreign.newCString "Snake"
  window   <- SDL.createWindow name 0 0 w h 0
  renderer <- SDL.createRenderer window (-1) SDL.rendererFlagAccelerated

  act renderer

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Foreign.free name

main :: IO ()
main = do
  SDL.init SDL.initFlagEverything
  state <- newIORef initialize
  withSDL $ \renderer -> do
    handler  <- mkEventFilter (eventHandler state)
    textures <- loadTextures renderer

    SDL.setEventFilter handler Foreign.nullPtr

    loop renderer textures state

    freeTextures textures

    SDL.quit
    Foreign.freeHaskellFunPtr handler
  where
    loop :: SDL.Renderer
         -> GameTextures
         -> IORef GameState
         -> IO ()
    loop renderer textures state = do
      state' <- readIORef state
      render renderer textures state'
      SDL.delay 250
      quit <- SDL.quitRequested
      over <- gameOver state'
      unless (quit || over) $ do
        modifyIORef state step
        loop renderer textures state
