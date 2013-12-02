{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Lens
import Control.Monad (forever)
import Data.Distributive (distribute)
import Foreign (Ptr, castPtr, nullPtr, sizeOf, with)
import Foreign.C (CFloat)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.SDL as SDL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as V
import Linear as L
import Linear ((!*!))

import Data.IORef

--------------------------------------------------------------------------------
triangleTransformation :: (Epsilon a, Floating a) => a -> M44 a
triangleTransformation =
  liftA2 (!*!) triangleTranslation triangleRotation

--------------------------------------------------------------------------------
triangleRotation :: (Epsilon a, Floating a) => a -> M44 a
triangleRotation t =
  m33_to_m44 $
    fromQuaternion $
      axisAngle (V3 0 1 0) (t * 2)

triangleTranslation :: Floating a => a -> M44 a
triangleTranslation t =
  eye4 & translation .~ V3 (sin t * 2) 0 (-5)

--------------------------------------------------------------------------------
main :: IO ()
main =
  SDL.withWindow "OpenGL Testing" (SDL.Position 0 0) (SDL.Size 800 600) [SDL.WindowOpengl] $ \win ->
  SDL.withOpenGL win $ do

  let z = 0
  let vertices = V.fromList [ 0, 1, 0
                            , -1, -1, z
                            , 1, -1, z ] :: V.Vector Float
      vertexAttribute = GL.AttribLocation 0

  cubeVao <- GL.genObjectName
  cubeVbo <- GL.genObjectName

  GL.bindVertexArrayObject $= Just cubeVao

  GL.bindBuffer GL.ArrayBuffer $= Just cubeVbo

  V.unsafeWith vertices $ \v -> GL.bufferData GL.ArrayBuffer $=
    (fromIntegral $ V.length vertices * sizeOf (0 :: Float), v, GL.StaticDraw)

  GL.vertexAttribPointer vertexAttribute $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

  GL.vertexAttribArray vertexAttribute $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just cubeVbo

  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader

  GL.shaderSourceBS vertexShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 130"
      , "uniform mat4 projection;"
      , "uniform mat4 model;"
      , "in vec3 in_Position;"
      , "void main(void) {"
      , "  gl_Position = projection * model * vec4(in_Position, 1.0);"
      , "}"
      ])

  GL.shaderSourceBS fragmentShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 130"
      , "out vec4 fragColor;"
      , "void main(void) {"
      , "  fragColor = vec4(1.0,1.0,1.0,1.0);"
      , "}"
      ])

  GL.compileShader vertexShader
  GL.compileShader fragmentShader

  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.attribLocation shaderProg "in_Position" $= vertexAttribute
  GL.linkProgram shaderProg
  GL.currentProgram $= Just shaderProg

  let fov = 90
      s = recip (tan $ fov * 0.5 * pi / 180)
      f = 1000
      n = 1

  let perspective = V.fromList [ s, 0, 0, 0
                              , 0, s, 0, 0
                              , 0, 0, -(f/(f - n)), -1
                              , 0, 0, -((f*n)/(f-n)), 1
                              ]

  GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
  V.unsafeWith perspective $ \ptr -> GL.glUniformMatrix4fv loc 1 0 ptr

  tr <- newIORef 0
  forever $ do
    t <- readIORef tr

    GL.clearColor $= GL.Color4 0.5 0.2 1 1
    GL.clear [GL.ColorBuffer]

    GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "model")
    with (distribute $ triangleTransformation t) $ \ptr ->
      GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

    GL.drawArrays GL.Triangles 0 3

    SDL.glSwapWindow win
    writeIORef tr (t + 0.1)
