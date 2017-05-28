-- Dijkstra
-- Cesar Rabelo de Almeida 141876
-- Rudolfo Goldmann Neto 139108

import System.IO
import Data.List

data No = No {
  nome::String,
  vizinhos::[(Char,Float)],
  cor::Char
} deriving(Show)

main = do
  file <- openFile "testes/teste0.in" ReadMode
  -- entrada <- hGetContents file
  entrada <- hGetLine file
  let
    final = reads $ entrada
  putStrLn final

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos

-- tst [] = []
tst ('a  b d':xs) = read x
