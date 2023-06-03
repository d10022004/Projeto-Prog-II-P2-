import Data.List (permutations)

type Exam = (Int, Int) -- (UC, Ano)

-- Função para verificar se dois exames estão no mesmo dia
sameDay :: Exam -> Exam -> Bool
sameDay (_, ano1) (_, ano2) = ano1 == ano2

-- Função para verificar se uma atribuição de exames parcial é válida
isValidPartialAssignment :: [(Exam, Int)] -> Bool
isValidPartialAssignment exams =
  all (\(exam, day) -> all (not . sameDay exam . fst) (filter ((== day) . snd) exams)) exams

-- Função para encontrar uma atribuição válida de exames utilizando backtracking
backtrack :: [Exam] -> [(Exam, Int)] -> Maybe [(Exam, Int)]
backtrack [] assignments = Just assignments -- todos os exames foram atribuídos
backtrack (exam:exams) assignments =
  let possibleDays = [1..5] -- assumindo que os exames podem ser agendados em um dos cinco dias da semana
  in case filter (isValidPartialAssignment . (: assignments)) [(exam, day) | day <- possibleDays] of
       [] -> Nothing -- nenhuma atribuição válida para este exame, retornar Nothing
       (validAssignment:_) -> backtrack exams (validAssignment : assignments) -- uma atribuição válida foi encontrada, continuar com o próximo exame

solveExamScheduling :: [Exam] -> Maybe [(Exam, Int)]
solveExamScheduling exams = backtrack exams []

-- Exemplo de uso
main :: IO ()
main = do
  let exams = [(1, 2022), (2, 2022), (3, 2023), (4, 2023), (5, 2024)]
  case solveExamScheduling exams of
    Just assignments -> putStrLn (show assignments)
    Nothing -> putStrLn "No solution found."
