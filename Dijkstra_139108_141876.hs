-- Dijkstra
-- Cesar Rabelo de Almeida 141876
-- Rudolfo Goldmann Neto 139108

import System.IO

data No = No {
  nome::String,
  vizinhos::[(Char,Float)],
  cor::Char
} deriving(Show)

main = do
  tst <- openFile "testes/teste0.in" ReadMode
  entrada <- hGetContents tst
  putStrLn("teste:" ++ entrada)

-- retorna lista de valores de vizinhos e distancias
getViz (No _ vizinhos _) = vizinhos
