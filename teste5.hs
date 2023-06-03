import Data.List (minimumBy)
import Data.Function (on)

type Exam = (Int, Int) -- (UC, Ano)
type Schedule = [(Exam, Int)] -- Lista de exames e seus dias de escalonamento

-- Função para calcular o número de incompatibilidades em um escalonamento
countIncompatibilities :: Schedule -> Int
countIncompatibilities schedule =
  let exams = map fst schedule
      incompatibilities = sum [1 | (exam1, day1) <- schedule, (exam2, day2) <- schedule, exam1 /= exam2, day1 == day2, fst exam1 /= fst exam2]
  in incompatibilities

-- Função auxiliar para obter o dia de um exame específico no escalonamento
getDay :: Exam -> Schedule -> Maybe Int
getDay exam schedule = lookup exam schedule

-- Função para encontrar o escalonamento com o menor número de incompatibilidades usando busca local
findBestSchedule :: [Exam] -> Schedule
findBestSchedule exams =
  let initialSchedule = zip exams [1..]
      bestSchedule = minimumBy (compare `on` countIncompatibilities) (generateNeighbors initialSchedule)
  in bestSchedule

-- Função para gerar vizinhos de um escalonamento trocando os dias de exames
generateNeighbors :: Schedule -> [Schedule]
generateNeighbors schedule =
  let exams = map fst schedule
  in [updateDay exam day schedule | (exam, day) <- schedule, day <- [1..length exams], Just day /= getDay exam schedule]

-- Função para atualizar o dia de um exame em um escalonamento
updateDay :: Exam -> Int -> Schedule -> Schedule
updateDay exam day schedule = map (\(e, d) -> if e == exam then (e, day) else (e, d)) schedule

-- Exemplo de uso
main :: IO ()
main = do
  let exams = [(1, 2023), (2, 2023), (3, 2023), (4, 2024), (5, 2024)]
      bestSchedule = findBestSchedule exams
  putStrLn "Escalonamento de exames:"
  mapM_ (\((uc, ano), dia) -> putStrLn $ "UC " ++ show uc ++ " - Ano " ++ show ano ++ ": Dia " ++ show dia) bestSchedule
  putStrLn $ "Número de incompatibilidades: " ++ show (countIncompatibilities bestSchedule)
