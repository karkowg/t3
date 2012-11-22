-- T3: Gerando tag clouds em Haskell

module Main where

import Text.Printf

type Point  = (Float,Float)
type Color  = (Int,Int,Int)
type Circle = (Point,Float)

imageWidth :: Int
imageWidth = 640

imageHeight :: Int
imageHeight = 640


-- Função principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
  strcontent <- readFile infile
  let pairs = map (span (/= ' ')) (lines strcontent)
      freqs = readInts (map snd pairs)
  writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
  putStrLn "Ok!"
  where 
    infile  = "dataset.txt"
    outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabeçalho, conteúdo e rodapé
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
  (svgViewBox w h) ++
  (concat (svgBubbleGen 0 0 dataset)) ++ "</svg>\n"


-- Gera a lista de círculos em formato SVG
svgBubbleGen:: Float -> Float -> [Int] -> [String]
svgBubbleGen _ _ []    = []
svgBubbleGen x y (h:t) =
  svgCircle ((dx, dy), r) (rToColor r) : svgBubbleGen (x+3) (y+3) t
  where
    dx    = paramX 1 x
    dy    = paramY 1 y
    (r:_) = radiusList (h:t)


-- Gera string representando um círculo em SVG
svgCircle :: Circle -> Color -> String
svgCircle ((x,y),ra) (r,g,b) =
  printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y ra r g b


-- Configura o viewBox da imagem e coloca um retângulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
  printf "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
         " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
  printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h


-- Calcula a distância entre dois pontos
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)


-- Verifica intersecção de círculos
intersect :: Circle -> Circle -> Bool
intersect ((x1,y1),r1) ((x2,y2),r2)
  | d >= r1 + r2 = False
  | otherwise    = True
  where d = distance (x1,y1) (x2,y2)


-- Equações paramétricas da espiral
paramX :: Float -> Float -> Float
paramX a t = a*t*(cos t) + 320

paramY :: Float -> Float -> Float
paramY a t = a*t*(sin t) + 320


-- Transforma lista de frequências em lista de raios
radiusList :: [Int] -> [Float]
radiusList []    = []
radiusList (h:t) = fToRadius h : radiusList t


-- Tradução de intervalos (frequência -> raio)
fToRadius :: Int -> Float
fToRadius n
  | n>0 && n<6       = 1
  | n>5 && n<11      = 3
  | n>10 && n<16     = 5
  | n>15 && n<21     = 7
  | n>20 && n<31     = 11
  | n>30 && n<36     = 13
  | n>35 && n<51     = 15
  | n>50 && n<71     = 17
  | n>70 && n<91     = 19
  | n>90 && n<121    = 21
  | n>120 && n<201   = 23
  | n>200 && n<251   = 25
  | n>250 && n<401   = 27
  | n>400 && n<426   = 29
  | n>425 && n<451   = 31
  | n>450 && n<501   = 35
  | n>500 && n<1001  = 40
  | n>1000 && n<1501 = 45
  | n>1500 && n<2001 = 50
  | otherwise        = 64


-- Tradução de intervalos (raio -> cor)
rToColor :: Float -> Color
rToColor 1  = (105,0,0)
rToColor 3  = (130,0,0)
rToColor 5  = (155,0,0)
rToColor 7  = (180,0,0)
rToColor 11 = (205,0,0)
rToColor 13 = (230,0,0)
rToColor 15 = (255,0,0)
rToColor 17 = (0,0,105)
rToColor 19 = (0,0,130)
rToColor 21 = (0,0,155)
rToColor 23 = (0,0,180)
rToColor 25 = (0,0,205)
rToColor 27 = (0,0,230)
rToColor 29 = (0,0,255)
rToColor 31 = (0,105,0)
rToColor 35 = (0,130,0)
rToColor 40 = (0,155,0)
rToColor 45 = (0,180,0)
rToColor 50 = (0,205,0)
rToColor _  = (0,230,0)