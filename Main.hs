{-# LANGUAGE TemplateHaskell #-}
module Main where

import Sprite

import Data.IORef
import Control.Monad (forM_, unless)
import Control.Lens
import System.IO
import System.Exit
import System.Random (randomRIO)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import Reactive.Banana
import Reactive.Banana.Frameworks

data GameScene = GameStart | GamePlaying | GameOver

data GameTextures = MkGameTextures { _texBird01   :: TextureObject
                                   , _texBird02   :: TextureObject
                                   , _texBird03   :: TextureObject
                                   , _texBird04   :: TextureObject
                                   , _texLand     :: TextureObject
                                   , _texSky      :: TextureObject
                                   , _texPipeUp   :: TextureObject
                                   , _texPipeDown :: TextureObject
                                   }

data GameStates = MkGameStates { _stScene :: GameScene
                               , _stTex   :: GameTextures
                               , _stBird  :: Sprite
                               , _stLand  :: Sprite
                               , _stSky   :: Sprite
                               , _stPipes :: [(Sprite, Sprite)]
                               }
makeLenses ''GameTextures
makeLenses ''GameStates

loadGameTextures :: IO GameTextures
loadGameTextures = MkGameTextures
               <$> loadGLTextureFromFile "img/bird-01.png"
               <*> loadGLTextureFromFile "img/bird-02.png"
               <*> loadGLTextureFromFile "img/bird-03.png"
               <*> loadGLTextureFromFile "img/bird-04.png"
               <*> loadGLTextureFromFile "img/land.png"
               <*> loadGLTextureFromFile "img/sky.png"
               <*> loadGLTextureFromFile "img/PipeUp.png"
               <*> loadGLTextureFromFile "img/PipeDown.png"

initGameStates :: IO (IORef GameStates)
initGameStates = do
    texs <- loadGameTextures
    newIORef MkGameStates { _stScene    = GamePlaying
                          , _stTex      = texs
                          , _stBird     = Sprite { _spTex   = texs ^. texBird01
                                                 , _spPos   = Vertex2 30 400
                                                 , _spSize  = Vector2 34 24
                                                 , _spSpeed = Vector2 0 0
                                                 }
                          , _stLand     = Sprite { _spTex   = texs ^. texLand
                                                 , _spPos   = Vertex2 0 0
                                                 , _spSize  = Vector2 336 112
                                                 , _spSpeed = Vector2 (-1) 0
                                                 }
                          , _stSky      = Sprite { _spTex   = texs ^. texSky
                                                 , _spPos   = Vertex2 0 112
                                                 , _spSize  = Vector2 276 109
                                                 , _spSpeed = Vector2 (-0.5) 0
                                                 }
                          , _stPipes    = []
                          }

main :: IO ()
main = do
    let errorCallback err description = hPutStrLn stderr description
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    if not successfulInit
        then exitFailure
        else do
            mw <- GLFW.createWindow 320 480 "FlappyHaskell" Nothing Nothing
            case mw of
                Nothing -> (GLFW.terminate >> exitFailure)
                Just window -> do
                    GLFW.makeContextCurrent mw
                    preMainLoop window
                    GLFW.destroyWindow window
                    GLFW.terminate
                    exitSuccess

data KeyState = KeyReleased | KeyPressed | KeyRepeating deriving (Eq, Show)

setupNetwork :: AddHandler KeyState -> IORef GameStates -> IO EventNetwork
setupNetwork addHandler statesRef = compile $ do
    spaceKeyEvent <- fromAddHandler addHandler
    let foldKeyEvent = accumE KeyReleased $ diffKeyState <$> spaceKeyEvent
    let countEvent = accumE 0 $ (\_ n -> n + 1) <$> spaceKeyEvent

    let moveBird keyState = do
            states <- readIORef statesRef
            let bird = move $ if keyState == KeyPressed
                                  then states ^. stBird & spSpeed.y .~ 7
                                  else states ^. stBird & spSpeed.y -~ 0.5
            let bird' = bird & spPos.y %~ (max 0 . min (480 - (bird ^. spSize.y)))
            writeIORef statesRef (states & stBird .~ bird')
    let moveLand _ = do
            states <- readIORef statesRef
            let land  = move $ states ^. stLand
            let land' = if (land ^. spPos.x) < negate (land ^. spSize.x)
                            then land & spPos.x +~ (land ^. spSize.x)
                            else land
            writeIORef statesRef (states & stLand .~ land')
    let moveSky _ = do
            states <- readIORef statesRef
            let sky  = move $ states ^. stSky
            let sky' = if (sky ^. spPos.x) < negate (sky ^. spSize.x)
                            then sky & spPos.x +~ (sky ^. spSize.x)
                            else sky
            writeIORef statesRef (states & stSky .~ sky')
    let movePipes n = do
            states <- readIORef statesRef
            let pipes = states ^. stPipes
            let moved = map (\(up, down) -> (move up, move down)) pipes
            let pipes' = filter (\(up, _) -> (up^.spPos.x) > -60) $ moved
            pipes'' <- if n `mod` 200 /= 0
                          then return pipes'
                          else do
                              y <- randomRIO (160, 380)
                              let up   = Sprite { _spTex   = states^.stTex.texPipeUp
                                                , _spPos   = Vertex2 320 (y - 50 - 320)
                                                , _spSize  = Vector2 60 320
                                                , _spSpeed = Vector2 (-1) 0
                                                }
                              let down = up { _spTex   = states^.stTex.texPipeDown
                                            , _spPos   = Vertex2 320 (y + 50)
                                            }
                              return $ (up, down) : pipes'
            writeIORef statesRef (states & stPipes .~ pipes'')
    let birdFlap n = do
            states <- readIORef statesRef
            let bird' = case mod (div n 6) 4 of
                            0 -> states ^. stBird & spTex .~ (states ^. stTex.texBird01)
                            1 -> states ^. stBird & spTex .~ (states ^. stTex.texBird02)
                            2 -> states ^. stBird & spTex .~ (states ^. stTex.texBird03)
                            3 -> states ^. stBird & spTex .~ (states ^. stTex.texBird04)
            writeIORef statesRef (states & stBird .~ bird')
    let judge _ = do
            states <- readIORef statesRef
            let bird  = states ^. stBird
            let pipes = states ^. stPipes
            if any (\(up, down) -> hitTest bird up || hitTest bird down) pipes
                then putStr "\a"
                else return ()

    reactimate $ moveBird  <$> foldKeyEvent
    reactimate $ moveLand  <$> spaceKeyEvent
    reactimate $ moveSky   <$> spaceKeyEvent
    reactimate $ movePipes <$> countEvent
    reactimate $ judge     <$> countEvent
    reactimate $ birdFlap  <$> countEvent

preMainLoop :: GLFW.Window -> IO ()
preMainLoop window = do
    statesRef <- initGameStates

    (addHandler, fire) <- newAddHandler
    network <- setupNetwork addHandler statesRef
    actuate network

    -- Init OpenGL
    clearColor $= Color4 0.30078125 0.75 0.7890625 1.0
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blend $= Enabled
    texture Texture2D $= Enabled

    mainLoop statesRef fire window

mainLoop :: IORef GameStates -> (Handler KeyState) -> GLFW.Window -> IO ()
mainLoop statesRef fire window = do
    action <- (GLFW.windowShouldClose window)
    unless action $ do
        fire =<< convertKeyState <$> GLFW.getKey window GLFW.Key'Space

        (width, height) <- GLFW.getFramebufferSize window
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        clear [ColorBuffer, DepthBuffer]

        matrixMode $= Projection
        loadIdentity
        lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 0 1 0)

        let draw = drawInWindow width height
        states <- readIORef statesRef
        matrixMode $= Modelview 0
        case states ^. stScene of
            GameStart   -> do
                draw (states ^. stSky & spPos.x +~ (states ^. stSky.spSize.x))
                draw (states ^. stSky)
                draw (states ^. stLand)
                draw (states ^. stBird)
            GamePlaying -> do
                draw (states ^. stSky & spPos.x +~ (2 * (states ^. stSky.spSize.x)))
                draw (states ^. stSky & spPos.x +~ (states ^. stSky.spSize.x))
                draw (states ^. stSky)
                forM_ (states ^. stPipes) $ \(up, down) -> draw up >> draw down
                draw (states ^. stLand & spPos.x +~ (states ^. stLand.spSize.x))
                draw (states ^. stLand)
                draw (states ^. stBird)
            GameOver    -> do
                draw (states ^. stSky & spPos.x +~ (2 * (states ^. stSky.spSize.x)))
                draw (states ^. stSky & spPos.x +~ (states ^. stSky.spSize.x))
                draw (states ^. stSky)
                forM_ (states ^. stPipes) $ \(up, down) -> draw up >> draw down
                draw (states ^. stLand & spPos.x +~ (states ^. stLand.spSize.x))
                draw (states ^. stLand)
                draw (states ^. stBird)

        GLFW.swapBuffers window
        GLFW.pollEvents
        mainLoop statesRef fire window

convertKeyState :: GLFW.KeyState -> KeyState
convertKeyState GLFW.KeyState'Released  = KeyReleased
convertKeyState GLFW.KeyState'Pressed   = KeyPressed
convertKeyState GLFW.KeyState'Repeating = KeyRepeating

diffKeyState :: KeyState -> KeyState -> KeyState
diffKeyState KeyPressed KeyReleased  = KeyPressed
diffKeyState KeyPressed KeyPressed   = KeyRepeating
diffKeyState KeyPressed KeyRepeating = KeyRepeating
diffKeyState _            _          = KeyReleased
