-- Dijkstra
-- Cesar Rabelo de Almeida 141876
-- Rudolfo Goldmann Neto 139108

import System.IO
import Data.List

data No = No {
  nome::String,
  vizinhos::[(String,Float)],
  cor::Char
} deriving(Show, Read)

main = do
  file <- openFile "testes/teste0.in" ReadMode
  -- entrada <- hGetContents file
  entrada <- hGetContents file
  let
    pre =  getlist $ lines entrada
    path = concat [z | z <- pre, length z == 1]
    graph =  [z | z <- pre, length z == 3]
    -- final = getype pre
  print graph

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos

-- getlist::[String] -> [[String]]
getlist [] = []
getlist (x:xs) =
  words x : getlist xs

-- node_list::[[String]] -> [No]
-- node_list ((no:viz:val:[]):xs) =
--   if
