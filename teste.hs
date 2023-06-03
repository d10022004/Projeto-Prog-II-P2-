import Control.Monad
import Data.List

data Exame = Exame { uc :: String, sala :: String, dia :: String } deriving (Show, Eq, Ord)

gerarEscalonamento :: [String] -> [(String, String)] -> [Exame]
gerarEscalonamento ucs slots = zipWith (\uc slot -> Exame uc (fst slot) (snd slot)) ucs slots

verificarConflitos :: [Exame] -> Bool
verificarConflitos exames = length (nub exames) /= length exames

main :: IO ()
main = do
  let ucs = ["UC1", "UC2", "UC3", "UC4"]
  let salas = ["Sala1"]
  let dias = ["Segunda", "Terça"]
  let slots = [(sala, dia) | sala <- salas, dia <- dias]
  
  if length slots < length ucs
    then putStrLn "Número insuficiente de salas e/ou dias disponíveis para acomodar todos os exames."
    else do
        let escalonamento = gerarEscalonamento ucs slots
        if verificarConflitos escalonamento
          then putStrLn "Há conflitos no escalonamento dos exames."
          else writeFile "escalonamento.txt" (unlines $ map show escalonamento)
