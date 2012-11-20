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


-- Fun��o principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabe�alho, conte�do e rodap�
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Esta fun��o deve gerar a lista de c�rculos em formato SVG
-- A implementa��o atual � apenas um teste que gera um c�rculo posicionado no meio da figura
-- TODO: Alterar essa fun��o para usar os dados do dataset
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [svgCircle ((fromIntegral w/2, fromIntegral h/2), 10.0) (0,255,0)]


-- Gera string representando um c�rculo em SVG. A cor do c�rculo esta fixa. 
-- TODO: Alterar esta fun��o para mostrar um c�rculo de uma cor fornecida como par�metro.
svgCircle :: Circle -> Color -> String
svgCircle ((x,y),ra) (r,g,b) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y ra r g b


-- Configura o viewBox da imagem e coloca um ret�ngulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h


-- Calcula a dist�ncia entre dois pontos
calcDist :: Point -> Point -> Float
calcDist (x1,y1) (x2,y2) = sqrt (((x2-x1)^2) + ((y2-y1)^2))


-- Verifica intersec��o de c�rculos
intersect :: Circle -> Circle -> Bool
intersect ((x1,y1),r1) ((x2,y2),r2)
  | calcDist (x1,y1) (x2,y2) >= r1 + r2 = False
  | otherwise                           = True