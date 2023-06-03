import Data.List (permutations)

type Exam = (Int, Int) -- (UC, Ano)

-- Função para verificar se dois exames estão no mesmo dia
sameDay :: Exam -> Exam -> Bool
sameDay (_, ano1) (_, ano2) = ano1 == ano2

-- Função para verificar se uma atribuição de exames está válida
isValidAssignment :: [(Exam, Int)] -> Bool
isValidAssignment exams =
  all (\(exam, day) -> all (not . sameDay exam . fst) (filter ((== day) . snd) exams)) exams

-- Função para gerar todas as permutações possíveis das atribuições de exames
generatePermutations :: [Exam] -> [[(Exam, Int)]]
generatePermutations exams =
  filter isValidAssignment (permutations [(exam, day) | exam <- exams, day <- [1..]])

-- Função para resolver o problema de escalonamento de exames
solveExamScheduling :: [Exam] -> Maybe [(Exam, Int)]
solveExamScheduling exams =
  case generatePermutations exams of
    [] -> Nothing
    (x:_) -> Just x

-- Exemplo de uso
main :: IO ()
main = do
  let exams = [(1, 2022), (2, 2022), (3, 2023), (4, 2023), (5, 2024)]
  case solveExamScheduling exams of
    Just assignments -> putStrLn (show assignments)
    Nothing -> putStrLn "No solution found."


