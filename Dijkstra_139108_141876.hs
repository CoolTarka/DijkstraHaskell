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
    -- graph tem todos as especificaçoes de no e seus vizinhos
    graph = [z | z <- pre, length z == 3]
    -- nodes tem todos os nos do grafo
    nodes = get_nodes (map head graph) 0
    tst = pre_graph nodes graph
    tst2 = fst $ head $ head $ head tst
    tst3 = head $ tail $ head tst
    tst4 =  create_record tst
  print tst4

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos

-- pega a entrada no formato ["a b 4"] e passa pra ["a","b","4"]
-- getlist::[String] -> [[String]]
getlist [] = []
getlist (x:xs) =
  words x : getlist xs

create_record [] = []
create_record lista =
    No node vizinhos 'C' : create_record (tail lista)
    where
      node = fst ( head ( head ( head lista)))
      vizinhos = head ( tail ( head lista))

-- Organiza os valores pra por no grafo
pre_graph [] _ = []
pre_graph (x:xs) lista =
  [[(x,0)]:create_no x lista:[]] ++ pre_graph xs lista

-- retorna todos os vizinhos em uma lista de tuplas (vizinho,distancia) pra um no só
create_no node [] = []
create_no node ((no:viz:val:[]):xs) =
  if node == no then (viz, read val :: Float): create_no node xs
  else create_no node xs

-- pega todos elementos iguais consecutivos e mantem apenas um, tem um exmeplo ai em baixo
get_nodes [] _ = []
get_nodes (x:xs) val
   | null y = []
   | x == y && val == 0 = y : get_nodes xs 1
   | x /= y = y : get_nodes xs 1
   | otherwise  = get_nodes xs 1
   where
     y = if null xs then [] else head xs
-- EXEMPLO
-- get_nodes ["a","a","a","a","b","b","b","c","d","d","d","d","d","e"] 0
-- > ["a","b","c","d","e"]

-- #########################################################################
-- funcao de um broder mutante da internet, nao funciona aqui
  -- myGroup :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
  -- myGroup = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
  --           . sortBy (comparing fst)
