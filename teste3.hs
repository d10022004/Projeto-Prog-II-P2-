import Data.List (nub, intersect)
import Data.Maybe (fromMaybe)

type UC = Int
type Aluno = Int

-- Dados fictícios de inscrições dos alunos em cada UC
inscricoes :: [(UC, [Aluno])]
inscricoes = [(1, [1, 2, 3]), (2, [2, 3, 4]), (3, [3, 4, 5]), (4, [4, 5, 6]), (5, [5, 6, 7])]

-- Função auxiliar para buscar os alunos inscritos em uma UC
getAlunosInscritos :: UC -> [(UC, [Aluno])] -> [Aluno]
getAlunosInscritos uc lst = fromMaybe [] (lookup uc lst)

-- Função para calcular as incompatibilidades entre cada par de UCs
calcIncompatibilidades :: [(UC, [Aluno])] -> [(UC, UC, Int)]
calcIncompatibilidades inscricoes =
  let ucs = map fst inscricoes
      paresUCs = [(uc1, uc2) | uc1 <- ucs, uc2 <- ucs, uc1 /= uc2]
      incompatibilidades = map (\(uc1, uc2) -> (uc1, uc2, length (intersect (getAlunosInscritos uc1 inscricoes) (getAlunosInscritos uc2 inscricoes)))) paresUCs
  in incompatibilidades

-- Exemplo de uso
main :: IO ()
main = do
  let incompatibilidades = calcIncompatibilidades inscricoes
  putStrLn "Incompatibilidades entre pares de UCs:"
  mapM_ (\(uc1, uc2, incompat) -> putStrLn $ "UCs " ++ show uc1 ++ " e " ++ show uc2 ++ ": " ++ show incompat ++ " alunos inscritos em ambas") incompatibilidades
