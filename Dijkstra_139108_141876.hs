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

-- ######## a partir daqui até graph são todos passos intermediarios até a lista de estruturas No ######
    -- graph_esp tem todos as especificaçoes de no e seus vizinhos
    graph_esp = [z | z <- pre, length z == 3]
    -- nodes é um dicionario de triplas no formato (valor da distancia, no, no anterior)
    -- (essa função vai cagar no codigo se a entrada não tiver listado a entrada de cada nó junto)
    nodes = get_nodes (map head graph_esp) 0
    -- list_graph mosta uma lista no formato [[[("no1",0)],[vizinhos de "no1"]],[[("no2",0)],[vizinhos de "no2"]]...]
    list_graph = pre_graph nodes graph_esp
    -- graph é uma lista do grafo na estrutura No (acima)
    graph =  create_record list_graph
  print nodes

-- pega a entrada no formato ["a b 4"] e passa pra ["a","b","4"]
-- getlist::[String] -> [[String]]
getlist [] = []
getlist (x:xs) =
  words x : getlist xs

-- monta a lista da struct No
create_record [] = []
create_record lista =
    No node vizinhos 'C' : create_record (tail lista)
    where
      node = fst ( head ( head ( head lista)))
      vizinhos = head ( tail ( head lista))

-- Organiza os valores pra por no grafo
pre_graph [] _ = []
pre_graph ((_,x,_,_):xs) lista =
  [[(x,0)]:create_no x lista:[]] ++ pre_graph xs lista

-- retorna todos os vizinhos em uma lista de tuplas (vizinho,distancia) pra um no só
create_no node [] = []
create_no node ((no:viz:val:[]):xs) =
  if node == no then (viz, read val :: Float): create_no node xs
  else create_no node xs

-- pega todos elementos iguais consecutivos e mantem apenas um
-- criando um dicicionario pra ser usado mais tarde
get_nodes [] _ = []
get_nodes (x:xs) val
   | null y = []
   | x == y && val == 0 = (-1.0, y, "nd", 'C') : get_nodes xs 1
   | x /= y = (-1.0, y, "nd", 'C') : get_nodes xs 1
   | otherwise  = get_nodes xs 1
   where
     y = if null xs then [] else head xs
-- EXEMPLO
-- get_nodes ["a","a","a","a","b","b","b","c","d","d","d","d","d","e"] 0
-- > [("a","nd",'C'),(-1,"b","nd",'C'),(-1,"c","nd",'C),(-1,"d","nd",'C),(-1,"e","nd",'C')]

-- ######################### FUNCOES DIJKSTRA ############################

--  ESTRUTURA DA QUADRUPLA DO DICIONARIO
-- (Distancia do elemento inicial ate esse nó, EsseNoó, nó que chega a esse por essa distancia, cor)
-- Exemplo: grafo a -- 4.0 --> b -- 5.2 --> c := [(-1,"a","nd",'C'),(4.0,"b","a",'C'),(9.2,"c","b",'C')]

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos
-- retorna o no
getNode (No node _ _) = node
-- retorna a cor, possivelmente inutil
getColor (No _ _ color) = color

-- nao existe implementada no sistema pra quadrupla
fst4 (x, _, _, _) = x
snd4 (_, x, _, _) = x
trd4 (_, _, x, _) = x
qth4 (_, _, _, x) = x

-- retorna a struct de um no especifico
get_record:: [No] -> String -> No
get_record [] _ = No "naoAchouSeuNo" [] 'N'
get_record (x:xs) node =
  if getNode x == node then x
  else get_record xs node


-- futura função que vai coordenar as outras
-- dijkstra::[String] -> [No] ->
-- dijkstra path graph =


-- passa todos os vizinhos de um no para update_paths onde eles sao registrados no dicionarios e o no é pitado de branco
assign_neighbors:: [(String, Float)] -> (Float, String, String, Char) -> [(Float, String, String, Char)] -> [(Float, String, String, Char)]
assign_neighbors [] _ dic = dic
assign_neighbors (x:xs) nome dic =
  assign_neighbors xs nome novo_dic
  where
    no = fst x
    dist = snd x
    novo_dic = update_paths dic no dist nome


-- atualiza o valor de distancia no dicionario de nós e pinta de branco o nó de que partiu a atualização
update_paths::[(Float, String, String, Char)] -> String -> Float -> (Float, String, String, Char) -> [(Float, String, String, Char)]
update_paths [] _ _ _ = []
update_paths (x:xs) node dist_actualNode from
  | actual_node == node && current_dist /= -1.0 =
      if chk_dist < current_dist then (chk_dist, snd4 x, from_node, qth4 x) : update_paths xs node dist_actualNode from
      else (current_dist, snd4 x, from_node, qth4 x) : update_paths xs node dist_actualNode from
  | actual_node == node = (chk_dist, snd4 x, from_node, qth4 x) : update_paths xs node dist_actualNode from
  | actual_node == from_node && qth4 x /= 'B' = (fst4 x, snd4 x, trd4 x, 'B') : update_paths xs node dist_actualNode from
  | otherwise =  x:update_paths xs node dist_actualNode from
  where
    from_node = snd4 from
    pre_dist = fst4 from
    actual_node = snd4 x
    chk_dist = if pre_dist == -1 then  dist_actualNode else dist_actualNode + pre_dist
    current_dist = fst4 x

-- pega o proximo nó para o algoritomo analisar os vizinhos
-- ou seja, procura o menor valor no dicionario que não seja Branco
next_node::[(Float, String, String, Char)] -> (Float, String, String, Char)
next_node dic =
  minimum $ filter (\ (y, _, _, x) -> x /= 'B' && y /= -1) dic
