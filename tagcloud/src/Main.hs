-- T3: Gerando tag clouds em Haskell

module Main where

import Text.Printf
import System.Random
import System.IO.Unsafe

type Point  = (Float,Float)
type Pair   = (Float,Float)
type Color  = (Int,Int,Int)
type Circle = (Point,Pair)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


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


-- Gera um número random a partir de um valor mínimo e um máximo
getRandom :: Int -> Int -> Int
getRandom lo hi = unsafePerformIO (getStdRandom (randomR (lo, hi)))


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabeçalho, conteúdo e rodapé
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
  (svgViewBox w h) ++
  (concat (svgBubbleGen dataset)) ++ "</svg>\n"


-- Gera a lista de círculos em formato SVG
svgBubbleGen:: [Int] -> [String]
svgBubbleGen []      = []
svgBubbleGen dataset = sbgAux cl []
  where
    rl = radiusList dataset
    cl = circleList rl

sbgAux :: [Circle] -> [Circle] -> [String]
sbgAux [] circles = svgCircleGen (resetCoord circles)
sbgAux (c1:t1) [] = sbgAux t1 [c1]
sbgAux (c1:t1) circles
  | flag == True = sbgAux ([newc1] ++ t1) circles
  | otherwise    = sbgAux t1 (c1 : circles)
  where
    newc1 = setCoord c1
    flag  = intersect c1 circles

svgCircleGen :: [Circle] -> [String]
svgCircleGen []    = []
svgCircleGen (c:t) = svgCircle c : svgCircleGen t


-- Equações paramétricas da espiral
paramX :: Float -> Float -> Float
paramX a t = a*t*(cos t)

paramY :: Float -> Float -> Float
paramY a t = a*t*(sin t)


-- Calcula coordenadas de um círculo
setCoord :: Circle -> Circle
setCoord ((_,_),(r,e)) = ((dx,dy),(r,t))
  where
    t  = e+1
    a  = 0.1
    dx = paramX a t
    dy = paramY a t


-- Reseta as posições de acordo com as dimensões da view box
rcAux :: [Circle] -> [Circle]
rcAux []    = []
rcAux (c:t) = ((dx,dy),(r,e)) : rcAux t
  where
    ((x,y),(r,e)) = c
    dx = x+180
    dy = y+180

resetCoord :: [Circle] -> [Circle]
resetCoord circles = sort (rcAux circles)


-- Ordena os círculos pelo raio (decrescente)
insert :: Circle -> [Circle] -> [Circle]
insert c1 [] = c1 : []
insert c1 (c2:t)
  | r1 > r2   = c1 : (c2:t)
  | otherwise = c2 : insert c1 t
  where
    r1 = snd c1
    r2 = snd c2

sort :: [Circle] -> [Circle]
sort []    = []
sort (h:t) = insert h (sort t)


-- Transforma lista de raios em lista de círculos (posição inicial (0,0), e = 0 rad)
circleGen :: Point -> Pair -> Circle
circleGen (x,y) (r,e) = ((x,y),(r,e))

circleList :: [Float] -> [Circle]
circleList []    = []
circleList (r:t) = circleGen (0,0) (r,0) : circleList t


-- Gera string representando um círculo em SVG
svgCircle :: Circle -> String
svgCircle ((x,y),(r,_)) =
  printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r re gr bl
  where
    lo = 0
    hi = 255
    re = getRandom lo hi
    gr = getRandom lo hi
    bl = getRandom lo hi


-- Configura o viewBox da imagem e coloca um retângulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
  printf "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
         " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
  printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h


-- Calcula a distância entre dois pontos
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2-x1
    dy = y2-y1


-- Verifica intersecção entre círculos
intersect :: Circle -> [Circle] -> Bool
intersect _ [] = False
intersect c1 (c2:t)
  | d >= r1+r2+l = intersect c1 t
  | otherwise    = True
  where
    (p1,(r1,_)) = c1
    (p2,(r2,_)) = c2
    l = 0.2
    d = distance p1 p2


-- Transforma lista de frequências em lista de raios
radiusList :: [Int] -> [Float]
radiusList []    = []
radiusList (h:t) = fToRadius h : radiusList t


-- Tradução de intervalos (frequência -> raio)
fToRadius :: Int -> Float
fToRadius f = fromIntegral f/50 + 2