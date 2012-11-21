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
  (concat (svgBubbleGen dataset)) ++ "</svg>\n"


-- Esta função deve gerar a lista de círculos em formato SVG
-- A implementação atual é apenas um teste que gera um círculo posicionado no meio da figura
-- TODO: Alterar essa função para usar os dados do dataset
svgBubbleGen:: [Int] -> [String]
svgBubbleGen dataset = [svgCircle ((paramX 0,paramY 0), 17) (rToColor 17),
  svgCircle ((paramX 30,paramY 30), 13) (rToColor 13),
  svgCircle ((paramX 35,paramY 35), 13) (rToColor 13),
  svgCircle ((paramX 40,paramY 40), 13) (rToColor 13),
  svgCircle ((paramX 45,paramY 45), 11) (rToColor 11),
  svgCircle ((paramX 50,paramY 50), 11) (rToColor 11),
  svgCircle ((paramX 55,paramY 55), 11) (rToColor 11),
  svgCircle ((paramX 60,paramY 60), 7) (rToColor 7),
  svgCircle ((paramX 62,paramY 62), 7) (rToColor 7),
  svgCircle ((paramX 64,paramY 64), 7) (rToColor 7),
  svgCircle ((paramX 65,paramY 65), 5) (rToColor 5),
  svgCircle ((paramX 66,paramY 66), 5) (rToColor 5),
  svgCircle ((paramX 67,paramY 67), 5) (rToColor 5),
  svgCircle ((paramX 68,paramY 68), 3) (rToColor 3),
  svgCircle ((paramX 69,paramY 69), 3) (rToColor 3),
  svgCircle ((paramX 70,paramY 70), 3) (rToColor 3)]


-- Gera string representando um círculo em SVG. A cor do círculo esta fixa. 
-- TODO: Alterar esta função para mostrar um círculo de uma cor fornecida como parâmetro.
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
distance (x1,y1) (x2,y2) = sqrt (((x2-x1)^2) + ((y2-y1)^2))


-- Verifica intersecção de círculos
intersect :: Circle -> Circle -> Bool
intersect ((x1,y1),r1) ((x2,y2),r2)
  | distance (x1,y1) (x2,y2) >= r1 + r2 = False
  | otherwise                           = True


-- Equações paramétricas da espiral
paramX :: Float -> Float
paramX ra = 1*ra*(cos ra) + 320

paramY :: Float -> Float
paramY ra = 1*ra*(sin ra) + 320

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
rToColor 0  = (0,0,0)
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