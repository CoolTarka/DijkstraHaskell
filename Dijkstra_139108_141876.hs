-- Dijkstra
-- Cesar Rabelo de Almeida 141876
-- Rudolfo Goldmann Neto 139108

import System.IO
import Data.List
import Data.Function
import Data.Ord
Import Control.Monad

data No = No {
  nome::String,
  vizinhos::[(String,Float)]
  -- cor::Char
} deriving(Show, Read)

main = do
  file <- openFile "testes/teste0.in" ReadMode
  entrada <- hGetContents file
  let
    pre =  map words $ lines entrada
    -- pre =  getlist $ lines entrada
    -- path tem o caminho a ser procurado pelo codigo
    path = concat [z | z <- pre, length z == 1]
    origem = head path
    destino = head $ tail path
-- ######## a partir daqui até graph são todos passos intermediarios até termos a lista de estruturas No ######
    -- graph_esp tem todos as especificaçoes de no e seus vizinhos
    graph_esp = [z | z <- pre, length z == 3]
    -- nodes é um dicionario de quadruplas no formato (valor da distancia, no, no anterior, cor)
    -- nub remove repetições, mantendo só 1 elemento
    origens = nub $ map head graph_esp
    destinos = nub $ map head $ map tail graph_esp
    list_nodes = nub $ origens ++ destinos
    -- cria lista de quadruplas
    nodes = new_get_nodes list_nodes origem
    -- list_graph mosta uma lista no formato [[[("no1",0)],[vizinhos de "no1"]],[[("no2",0)],[vizinhos de "no2"]]...]
    list_graph = pre_graph nodes graph_esp
    -- graph é uma lista do grafo na estrutura No (acima)
    graph =  create_record list_graph
    -- final é a aplicação do djikstra, lol
    pos_dijkstra = dijkstra origem graph nodes
    final_path = obtain_path pos_dijkstra origem destino
    cost = fst4 $ get_control pos_dijkstra destino
    caminho = unwords final_path
    -- when (not $ null final_path) $ do
    if (head final_path) == "nada"
      then do
        putStrLn("inical: " ++ origem)
        putStrLn("final: " ++ destino)
        putStrLn("nada")
        else do
          putStrLn("inical: " ++ origem)
          putStrLn("final: " ++ destino)
          putStr("custo: ")
          print cost
          putStrLn(caminho)
    -- putStr("naoseicaralho")

-- pega a entrada no formato ["a b 4"] e passa pra ["a","b","4"]
getlist::[String] -> [[String]]
getlist [] = []
getlist (x:xs) =
  words x : getlist xs

-- monta a lista da struct No
create_record [] = []
create_record lista =
    No node vizinhos : create_record (tail lista)
    where
      node = fst ( head ( head ( head lista)))
      vizinhos_pre = head ( tail ( head lista))
      vizinhos = if vizinhos_pre == [] then [("fksgsdg", 0)] else vizinhos_pre

-- Organiza os valores pra por no grafo
pre_graph [] _ = []
pre_graph ((_,x,_,_):xs) lista =
  [[(x,0)]:create_no x lista:[]] ++ pre_graph xs lista

-- retorna todos os vizinhos em uma lista de quadruplas (distancia, no, no de origem, cor) pra um no só
create_no node [] = []
create_no node ((no:viz:val:[]):xs) =
  if node == no then (viz, read val :: Float): create_no node xs
  else create_no node xs

-- pega todos elementos iguais consecutivos e mantem apenas um
-- criando um dicicionario pra ser usado mais tarde
new_get_nodes::[String] -> String -> [(Float, String, String, Char)]
new_get_nodes (x:[]) begin_node =
  if begin_node == x then (0.0, x, "nd", 'C'):[]
  else (1/0, x, "nd", 'C'):[]
new_get_nodes (x:xs) begin_node =
  if begin_node == x then (0.0, x, "nd", 'C'):new_get_nodes xs begin_node
  else (1/0, x, "nd", 'C'):new_get_nodes xs begin_node


obtain_path::[(Float, String, String, Char)] -> String -> String -> [String]
obtain_path _ _ "nd" = ["nada"]
obtain_path control origin destiny =
  if node /= origin then obtain_path control origin next_step ++ [node]
  else [node]
  where
    control_node = get_control control destiny
    dist = fst4 control_node
    node = snd4 control_node
    next_step = trd4 control_node


-- ######################### FUNCOES DIJKSTRA ############################

--  ESTRUTURA DA QUADRUPLA DO DICIONARIO
-- (Distancia do elemento inicial ate esse nó, EsseNoó, nó que chega a esse pela distancia, cor)
-- Exemplo: grafo a -- 4.0 --> b -- 5.2 --> c = [(-1,"a","nd",'C'),(4.0,"b","a",'C'),(9.2,"c","b",'C')]

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos) = vizinhos
-- retorna o no
getNode (No node _) = node

-- nao existe implementada no sistema pra quadrupla
fst4 (x, _, _, _) = x
snd4 (_, x, _, _) = x
trd4 (_, _, x, _) = x
qth4 (_, _, _, x) = x

-- retorna a struct de um no especifico (record é struct em haskell)
get_record:: [No] -> String -> No
get_record [] _ = No "naoAchouSeuNo" []
get_record (x:xs) node =
  if getNode x == node then x
  else get_record xs node

-- busca no controle/"dicionario", retornando o correpondente de quadrupla do dado nó
get_control::[(Float, String, String, Char)] -> String -> (Float, String, String, Char)
get_control [] _ = (-2.0, "justNothin","",'N')
get_control (x:xs) node =
  if node == snd4 x then x else get_control xs node

-- função que coordena as outras
-- No de inicio/No a ser iterado -> Lista das structs -> Lista de controle/"Dicionario"
dijkstra::String -> [No] -> [(Float, String, String, Char)] -> [(Float, String, String, Char)]
dijkstra [] _ control = control
dijkstra node_init graph control =
  dijkstra new_node graph new_neighbors
  where
    node = get_control control node_init
    neighbors = getViz $ get_record graph node_init
    new_node = if check_node /= (-2,"","",'N') then snd4 check_node else []
    check_node = next_node control
    new_neighbors = assign_neighbors neighbors node control


-- passa todos os vizinhos de um no para update_paths onde eles sao registrados no "dicionario" e o no é pintado de branco
-- Lista de todos os vizinhos do no atual -> no atual no formato de quadrupla -> lista de todas quadruplas -> lista de quad atualizada
assign_neighbors:: [(String, Float)] -> (Float, String, String, Char) -> [(Float, String, String, Char)] -> [(Float, String, String, Char)]
assign_neighbors [] _ control = control
assign_neighbors (x:xs) node control =
    assign_neighbors xs node new_control
  where
    neighbor = fst x
    dist = snd x
    new_control = update_paths control neighbor dist node

-- O CORAÇÃO DE DIJKSTRA
-- atualiza o valor de distancia no "dicionario" de nós e pinta de branco o nó de que partiu a atualização
-- lista de todas quadruplas -> nome de um vizinho do no atual -> distancia do no atual até esse vizinho -> quadrupla do no atual
update_paths::[(Float, String, String, Char)] -> String -> Float -> (Float, String, String, Char) -> [(Float, String, String, Char)]
update_paths [] _ _ _ = []
update_paths (x:xs) neighbor dist_neighbor from
  | current_node == neighbor =
      if chk_dist < current_dist then (chk_dist, current_node, from_node, current_color) : update_paths xs neighbor dist_neighbor from
      else (current_dist, current_node, current_orign, current_color) : update_paths xs neighbor dist_neighbor from
  | current_node == from_node && current_color /= 'B' = (current_dist, current_node, current_orign, 'B') : update_paths xs neighbor dist_neighbor from
  -- | current_node == neighbor = (chk_dist, current_node, from_node, current_color) : update_paths xs neighbor dist_neighbor from
  | otherwise =  x:update_paths xs neighbor dist_neighbor from
  where
    current_orign = trd4 x
    current_color = qth4 x
    pre_dist = fst4 from
    from_node = snd4 from
    current_dist = fst4 x
    current_node = snd4 x
    -- chk_dist = if pre_dist == 1/0 then  dist_neighbor else dist_neighbor + pre_dist
    chk_dist = dist_neighbor + pre_dist

-- pega o proximo nó para o algoritomo analisar os vizinhos
-- ou seja, procura o menor valor no dicionario que não seja Branco
next_node::[(Float, String, String, Char)] -> (Float, String, String, Char)
next_node control =
  if is_whited control == True then (-2,"","",'N')
  else minimum $ filter (\ (y, _, _, x) -> x /= 'B') control

-- checa se todos os nos do "dicionario" são brancos, evitando exception que o minimum gera
is_whited::[(Float,String, String, Char)] -> Bool
is_whited [] = True
is_whited (x:xs) =
  if color /= 'B' then False else is_whited xs
  where color = qth4 x
