{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module SceneParser where

import Text.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Number
import Control.Applicative hiding (many)
import Data.Vect.Float.Base

import Lights
import Surfaces
import Scene
import Types

type Parser = Parsec String ()

data SceneCommand
	= SC_Break
	| SC_Size Int Int
	| SC_MaxDepth Int
	| SC_Output String
	| SC_Camera Vec3 Vec3 Vec3 Float
	| SC_Sphere Vec3 Float
	| SC_MaxVerts Int
	| SC_Vertex Vec3
	| SC_Tri Int Int Int
	| SC_Translate Vec3
	| SC_Rotate Vec3 Float
	| SC_Scale Vec3
	| SC_PushTransform
	| SC_PopTransform
	| SC_DirectionalLight Vec3 Color
	| SC_PointLight Vec3 Color
	| SC_Attenuation Float Float Float
	| SC_Ambient Color
	| SC_Diffuse Color
	| SC_Specular Color
	| SC_Shininess Float
	| SC_Emission Color
	deriving Show

data MaterialConfig = MaterialConfig
	{ emission :: Color
	, diffuse :: Color
	, specular :: Color
	, shininess :: Float
	} deriving Show

-- default values

defaultSceneConfig = SceneConfig
	{ height = 1600
	, width = 900
	, maxDepth = 5
	, output = "output.bmp"
	, camera = Camera zero 0 0 zero zero zero
	, ambient = Color 0 0 0
	, surfaces = []
	, lights = []
	}

defaultMaterialConfig = MaterialConfig
	{ emission = Color 0 0 0
	, diffuse = Color 0 0 0
	, specular = Color 0 0 0
	, shininess = 0
	}

defaultAttenuation = Attenuation 1 0 0

-- unit parsers

parseFloat :: Parser Float
parseFloat = spaces >> sign <*> floating3 True

parseInt :: Parser Int
parseInt = spaces >> int

parseFilename :: Parser String
parseFilename = spaces >> many1 (noneOf "\n\t\"<>|/\\?*: ")

parseVec3 :: Parser Vec3
parseVec3 = spaces >> Vec3 <$> parseFloat <*> parseFloat <*> parseFloat

parseColor :: Parser Color
parseColor = spaces >> Color <$> parseFloat <*> parseFloat <*> parseFloat

-- command parsers

parseBreak :: Parser SceneCommand
parseBreak = string "break" >> pure SC_Break

parseSize = string "size" >> SC_Size <$> parseInt <*> parseInt
parseMaxDepth = string "maxdepth" >> SC_MaxDepth <$> parseInt
parseOutput = string "output" >> SC_Output <$> parseFilename
parseCamera = string "camera" >> SC_Camera <$> parseVec3 <*> parseVec3 <*> parseVec3 <*> parseFloat

parseSphere = string "sphere" >> SC_Sphere <$> parseVec3 <*> parseFloat
parseMaxVerts = string "maxverts" >> SC_MaxVerts <$> parseInt
parseVertex = string "vertex" >> SC_Vertex <$> parseVec3
parseTri = string "tri" >> SC_Tri <$> parseInt <*> parseInt <*> parseInt

parseTranslate = string "translate" >> SC_Translate <$> parseVec3
parseRotate = string "rotate" >> SC_Rotate <$> parseVec3 <*> parseFloat
parseScale = string "scale" >> SC_Scale <$> parseVec3
parsePushTransform = string "pushTransform" >> pure SC_PushTransform
parsePopTransform = string "popTransform" >> pure SC_PopTransform

parseDirectionalLight = string "directional" >> SC_DirectionalLight <$> parseVec3 <*> parseColor
parsePointLight = string "point" >> SC_PointLight <$> parseVec3 <*> parseColor

parseAttenuation = string "attenuation" >> SC_Attenuation <$> parseFloat <*> parseFloat <*> parseFloat

parseAmbient = string "ambient" >> SC_Ambient <$> parseColor
parseDiffuse = string "diffuse" >> SC_Diffuse <$> parseColor
parseSpecular = string "specular" >> SC_Specular <$> parseColor
parseShininess = string "shininess" >> SC_Shininess <$> parseFloat
parseEmission = string "emission" >> SC_Emission <$> parseColor

parseCommand :: Parser SceneCommand
parseCommand = choice $ map try $
	[ parseBreak
	, parseSize
	, parseMaxDepth
	, parseOutput
	, parseCamera
	, parseSphere
	, parseMaxVerts
	, parseVertex
	, parseTri
	, parseTranslate
	, parseRotate
	, parseScale
	, parsePushTransform
	, parsePopTransform
	, parseDirectionalLight
	, parsePointLight
	, parseAttenuation
	, parseAmbient
	, parseDiffuse
	, parseSpecular
	, parseShininess
	, parseEmission
	]

-- functions

update :: [SceneCommand] -> [Mat4] -> [Vec3] -> MaterialConfig -> Attenuation -> SceneConfig -> SceneConfig
update [] _ _ _ _ scene = scene

update (SC_Break : xs) _ _ _ _ scene = scene

update (SC_Translate v                        : xs) (t:ts) vs mat att scene = update xs ((t .*. translationMatrix v) : ts)                  vs mat att scene
update (SC_Rotate v degrees                   : xs) (t:ts) vs mat att scene = update xs ((t .*. rotationMatrix degrees (normalize v)) : ts) vs mat att scene
update (SC_Scale v                            : xs) (t:ts) vs mat att scene = update xs ((t .*. scalingMatrix v) : ts)                      vs mat att scene
update (SC_PushTransform                      : xs) (t:ts) vs mat att scene = update xs (t:t:ts)                                            vs mat att scene
update (SC_PopTransform                       : xs) (_:ts) vs mat att scene = update xs (ts)                                                vs mat att scene

update (SC_Emission color                     : xs) ( ts ) vs mat att scene = update xs ts vs (mat { emission = color }) att scene
update (SC_Diffuse color                      : xs) ( ts ) vs mat att scene = update xs ts vs (mat { diffuse = color })  att scene
update (SC_Specular color                     : xs) ( ts ) vs mat att scene = update xs ts vs (mat { specular = color }) att scene
update (SC_Shininess f                        : xs) ( ts ) vs mat att scene = update xs ts vs (mat { shininess = f })    att scene

update (SC_Attenuation const lin quad         : xs) ( ts ) vs mat att scene = update xs ts vs mat (Attenuation const lin quad) scene

update (SC_Size w h                           : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene { width = w, height = h })
update (SC_MaxDepth i                         : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene { maxDepth = i })
update (SC_Output filename                    : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene { output = filename })
update (SC_Camera lookFrom lookAt lookUp fovy : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene { camera = buildCamera lookFrom lookAt lookUp fovy (width scene) (height scene) })
update (SC_Ambient color                      : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene { ambient = color })

update (SC_MaxVerts i                         : xs) ( ts ) vs mat att scene = update xs ts vs mat att scene
update (SC_Vertex p                           : xs) ( ts ) vs mat att scene = update xs ts (vs++[p]) mat att scene
update (SC_Sphere p radius                    : xs) (t:ts) vs mat att scene = update xs (t:ts) vs mat att (scene {surfaces = (surfaces scene) ++ [buildSphere p radius (getMaterial mat) t]})
update (SC_Tri i1 i2 i3                       : xs) (t:ts) vs mat att scene = update xs (t:ts) vs mat att (scene {surfaces = (surfaces scene) ++ [buildTriangle (vs!!i1) (vs!!i2) (vs!!i3) (getMaterial mat) t]})

update (SC_DirectionalLight v color           : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene {lights = (lights scene) ++ [buildDirectionalLight v color]})
update (SC_PointLight p color                 : xs) ( ts ) vs mat att scene = update xs ts vs mat att (scene {lights = (lights scene) ++ [buildPointLight p color att]})

update _ _ _ _ _ scene = error "unknown command!"

getMaterial :: MaterialConfig -> Material
getMaterial MaterialConfig {..} = Material emission diffuse specular shininess

parseSceneRaw :: Parser [SceneCommand]
parseSceneRaw = sepEndBy parseCommand (char '\n')

parseScene :: Parser SceneConfig
parseScene = do
	commands <- parseSceneRaw
	let sceneConfig = update commands [diag (Vec4 1 1 1 1)] [] defaultMaterialConfig defaultAttenuation defaultSceneConfig
	return $ sceneConfig { camera = updateCamera (camera sceneConfig) (width sceneConfig) (height sceneConfig)}

fromRight :: Either a b -> b
fromRight (Left val) = error "fromRight: Either contains Left value, not Right!"
fromRight (Right val) = val

loadSceneFileAsLines :: String -> IO [String]
loadSceneFileAsLines filename = do
	x <- readFile filename
	let fileAsLines = map unwords $ map words $ lines x
	return $ filter (\line -> not (null line) && line!!0 /= '#') fileAsLines

-- for tests only
rawScene :: String -> IO [SceneCommand]
rawScene filename = do
	fileAsLines <- loadSceneFileAsLines filename
	let scene = fromRight $ parse parseSceneRaw "" (unlines fileAsLines)
	print scene
	return scene

loadSceneFile :: String -> IO SceneConfig
loadSceneFile filename = do
	fileAsLines <- loadSceneFileAsLines filename
	let scene = fromRight $ parse parseScene "" (unlines fileAsLines)
	return scene
