-- Dijkstra
-- Cesar Rabelo de Almeida 141876
-- Rudolfo Goldmann Neto 139108

import System.IO
import Data.List
import Data.Function
import Data.Ord

data No = No {
  nome::String,
  vizinhos::[(String,Float)],
  cor::Char
} deriving(Show, Read)

main = do
  file <- openFile "testes/teste0.in" ReadMode
  entrada <- hGetContents file
  let
    pre =  getlist $ lines entrada
    -- path tem o caminho a ser procurado pelo codigo
    path = concat [z | z <- pre, length z == 1]
    -- graph tem todos as especificaçoes de no
    graph = [z | z <- pre, length z == 3]
  print graph

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos

-- pega a entrada no formato ["a b 4"] e passa pra ["a","b","4"]
-- getlist::[String] -> [[String]]
getlist [] = []
getlist (x:xs) =
  words x : getlist xs

-- retorna todos os vizinhos em uma lista de tuplas (vizinho,distancia) pra um no só
create_no node [] = []
create_no node ((no:viz:val:[]):xs) =
  if node == no then (viz, read val :: Integer): create_no node xs
  else []

-- funcao de um broder mutante da internet, nao funciona aqui
myGroup :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
myGroup = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
