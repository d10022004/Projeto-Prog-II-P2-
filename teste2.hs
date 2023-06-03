import Data.List()

type Exam = (String, Int) -- (Disciplina, Ano)

-- Função para verificar se duas disciplinas estão no mesmo ano
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

resolveexamesol:: [Exam] -> Maybe [(Exam, Int)]
resolveexamesol exams = backtrack exams []

-- Exemplo de uso
main :: IO ()
main = do
  let exams = [("UC1", 2), ("UC2", 1), ("UC3", 2), ("UC4", 3), ("UC5", 4)]
  case resolveexamesol exams of
    Just assignments -> putStrLn (show assignments)
    Nothing -> putStrLn "No solution found."
