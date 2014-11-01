{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

-- Needed libraries:
--cabal install bmp
--cabal install parsec-numbers
--cabal install vect

import Codec.BMP
import qualified Data.ByteString as BS

import Raytrace
import Types
import Scene
import SceneParser (loadSceneFile, rawScene)

----------------------------------------------------------------

main = do
	--scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene4-emission.test"
	--scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene4-diffuse.test"
	--scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene4-specular.test"
	--scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene5.test"
	scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene6.test"
	--scene@SceneConfig {width, height, output} <- loadSceneFile "./scenes/scene7.test"

	--s <- rawScene "./scenes/scene6.test"
	--print scene

	print "Ray tracing image..."

	let image = raytraceImage scene

	let rgba = imageToByteString image
	let bmp = packRGBA32ToBMP24 (fromIntegral width) (fromIntegral height) rgba
	let filename = (reverse $ drop 4 $ reverse output) ++ ".bmp"
	writeBMP filename bmp

	print "Done!"

imageToByteString :: [Color] -> BS.ByteString
imageToByteString image = BS.pack imageAsWord8Array
	where
		imageAsWord8Array = concatMap colorToWord8Array image
		colorToWord8Array (Color r g b) = [
			fromIntegral (floor ((min 1 r)*255)),
			fromIntegral (floor ((min 1 g)*255)),
			fromIntegral (floor ((min 1 b)*255)), 255]
