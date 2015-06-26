{-# LANGUAGE TemplateHaskell #-}

module Sprite (
    loadGLTextureFromFile,
    Sprite(..),
    draw, move, hitTest,
    spTex, spPos, spSize, spSpeed,
    vertX, vertY, vecX, vecY
) where

import Control.Lens
import Control.Monad.State (execState)

import Graphics.Rendering.OpenGL
import Graphics.GLUtil (readTexture, texture2DWrap)

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return t

data Sprite = Sprite {
                _spTex   :: TextureObject
              , _spPos   :: Vertex2 GLfloat
              , _spSize  :: Vector2 GLfloat
              , _spSpeed :: Vector2 GLfloat
              }

makeLenses ''Sprite

vertX :: Lens (Vertex2 GLfloat) (Vertex2 GLfloat) GLfloat GLfloat
vertX = lens (\(Vertex2 x _) -> x) (\(Vertex2 x y) x' -> Vertex2 x' y)

vertY :: Lens (Vertex2 GLfloat) (Vertex2 GLfloat) GLfloat GLfloat
vertY = lens (\(Vertex2 _ y) -> y) (\(Vertex2 x y) y' -> Vertex2 x y')

vecX :: Lens (Vector2 GLfloat) (Vector2 GLfloat) GLfloat GLfloat
vecX = lens (\(Vector2 x _) -> x) (\(Vector2 x y) x' -> Vector2 x' y)

vecY :: Lens (Vector2 GLfloat) (Vector2 GLfloat) GLfloat GLfloat
vecY = lens (\(Vector2 _ y) -> y) (\(Vector2 x y) y' -> Vector2 x y')

screenWidth  = 320
screenHeight = 480

draw :: Sprite -> IO ()
draw sp = do
    textureBinding Texture2D $= Just (sp ^.spTex)
    let (Vertex2 x y) = sp ^.spPos
    let (Vector2 w h) = sp ^.spSize
    preservingMatrix $ do
        renderPrimitive Quads $ do
            n 0 0 1
            t 0 0 >> v (2*x/screenWidth-1)      (2*(y+h)/screenHeight-1) 0
            t 0 1 >> v (2*x/screenWidth-1)      (2*y/screenHeight-1)     0
            t 1 1 >> v (2*(x+w)/screenWidth-1) (2*y/screenHeight-1)      0
            t 1 0 >> v (2*(x+w)/screenWidth-1) (2*(y+h)/screenHeight-1)  0
    where
    v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
    n x y z = normal (Normal3 x y z :: Normal3 GLfloat)
    t u v   = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)

move :: Sprite -> Sprite
move sp = (`execState` sp) $ do
    speedX <- use (spSpeed.vecX)
    speedY <- use (spSpeed.vecY)
    spPos.vertX += speedX
    spPos.vertY += speedY

hitTest :: Sprite -> Sprite -> Bool
hitTest s1 s2 = let Vertex2 xmin1 ymin1 = s1 ^. spPos
                    Vertex2 xmin2 ymin2 = s2 ^. spPos
                    Vector2 w1 h1 = s1 ^. spSize
                    Vector2 w2 h2 = s2 ^. spSize
                    xmax1 = xmin1 + w1
                    ymax1 = ymin1 + h1
                    xmax2 = xmin2 + w2
                    ymax2 = ymin2 + h2
                in not $ (xmin1 > xmax2) || (ymin1 > ymax2) || (xmax1 < xmin2) || (ymax1 < ymin2)
