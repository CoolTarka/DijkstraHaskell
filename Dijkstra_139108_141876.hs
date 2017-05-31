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
    -- nodes é um dicionario de quadruplas no formato (valor da distancia, no, no anterior, cor)
    -- (essa função vai cagar no codigo se a entrada não tiver listado a entrada de cada nó junto)
    nodes = get_nodes (map head graph_esp) 0
    -- list_graph mosta uma lista no formato [[[("no1",0)],[vizinhos de "no1"]],[[("no2",0)],[vizinhos de "no2"]]...]
    list_graph = pre_graph nodes graph_esp
    -- graph é uma lista do grafo na estrutura No (acima)
    graph =  create_record list_graph
    final = dijkstra (head path) graph False nodes
  print final

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
dijkstra::String -> [No] -> Bool -> [(Float, String, String, Char)] -> [(Float, String, String, Char)]
dijkstra [] _ _ control = control
dijkstra node_init graph flag control =
  if flag == False then dijkstra node_init graph True $ assign_neighbors neighbors node control
  else dijkstra new_node graph True $ assign_neighbors neighbors node control
  where
    node = get_quad control node_init
    neighbors = getViz $ get_record graph node_init
    new_node = if check_node /= (-2,"","",'N') then snd4 check_node else []
    check_node = next_node control

-- passa todos os vizinhos de um no para update_paths onde eles sao registrados no "dicionario" e o no é pintado de branco
assign_neighbors:: [(String, Float)] -> (Float, String, String, Char) -> [(Float, String, String, Char)] -> [(Float, String, String, Char)]
assign_neighbors [] _ dic = dic
assign_neighbors (x:xs) nome dic =
    assign_neighbors xs nome novo_dic
  where
    no = fst x
    dist = snd x
    novo_dic = update_paths dic no dist nome

get_quad::[(Float, String, String, Char)] -> String -> (Float, String, String, Char)
get_quad [] _ = (-2.0, "justNothin","",'N')
get_quad (x:xs) node =
  if node == snd4 x then x else get_quad xs node

-- O CORAÇÃO DE DIJKSTRA
-- atualiza o valor de distancia no dicionario de nós e pinta de branco o nó de que partiu a atualização
update_paths::[(Float, String, String, Char)] -> String -> Float -> (Float, String, String, Char) -> [(Float, String, String, Char)]
update_paths [] _ _ _ = []
update_paths (x:xs) neighbor dist_neighbor from
  | current_node == neighbor && current_dist /= -1.0 =
      if chk_dist < current_dist then (chk_dist, current_node, from_node, qth4 x) : update_paths xs neighbor dist_neighbor from
      else (current_dist, current_node, from_node, qth4 x) : update_paths xs neighbor dist_neighbor from
  | current_node == neighbor = (chk_dist, current_node, from_node, qth4 x) : update_paths xs neighbor dist_neighbor from
  | current_node == from_node && qth4 x /= 'B' = (current_dist, current_node, trd4 x, 'B') : update_paths xs neighbor dist_neighbor from
  | otherwise =  x:update_paths xs neighbor dist_neighbor from
  where
    pre_dist = fst4 from
    from_node = snd4 from
    current_dist = fst4 x
    current_node = snd4 x
    chk_dist = if pre_dist == -1 then  dist_neighbor else dist_neighbor + pre_dist
-- TESTE
-- let ent2 = [("c",1.2),("d",7.9),("f",2.3),("h",0.1)]
-- let ex2 = [(-1.0,"a","nd",'B'),(4.5,"b","a",'C'),(7.8,"c","a",'C'),(-1.0,"d","nd",'C'),(-1.0,"e","nd",'C'),(-1.0,"f","nd",'C'),(3.2,"h","a",'C')]
-- assign_neighbors ent2 (4.5,"b","a",'C') ex2


-- pega o proximo nó para o algoritomo analisar os vizinhos
-- ou seja, procura o menor valor no dicionario que não seja Branco
next_node::[(Float, String, String, Char)] -> (Float, String, String, Char)
next_node control =
  if is_whited control == True then (-2,"","",'N')
  else minimum $ filter (\ (y, _, _, x) -> x /= 'B' && y /= -1) control

is_whited::[(Float,String, String, Char)] -> Bool
is_whited [] = True
is_whited (x:xs) =
  if color /= 'B' then False else is_whited xs
  where color = qth4 x
