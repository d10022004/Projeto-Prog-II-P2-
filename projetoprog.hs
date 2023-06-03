module Exem where
import System.IO 
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad
import Data.List

opcoes :: IO Int
opcoes = do
    op <- getLine
    let ope = read op :: Int
    return ope


-- PAUSA NA FUNÇÃO ------------------------------------------
pause :: IO()
pause = do
    putStrLn "Pressione Enter para continuar..."
    hFlush stdout
    getLine
    return ()

-- Pag de apresentação -------------------------------------
apresentatrab :: IO ()
apresentatrab = do
    putStrLn "Programac0o Funcional" -- Meter disciplina, ano letivo, nome do projeto e nomes dos alunos
    putStrLn "      2022/2023   "
    putStrLn "  "
    putStrLn "Planificacao para a gestao de salas e exmes em Haskell (PARTE II)"
    putStrLn "  "
    putStrLn "Realizado por:"
    putStrLn "   *Carolina Machado al79359"
    putStrLn "   *David Fidalgo al79881"
    putStrLn "   *Joao Fraga al78400"
    return ()

funcaotrabalho :: IO()
funcaotrabalho = do
    putStrLn "ola"
    return ()


---- Funcoes que buscam as informações dos ficheiros e transforma cada linha de ficheiro em uma lista --------------------------------------------
toDisciplinas :: [String] -> [(Int, Int, String)]
toDisciplinas = map toDisciplina


toDisciplina :: String -> (Int, Int, String)
toDisciplina str = case words str of
                     (n1:n2:rest) -> (read n1, read n2, unwords rest)
                     _ -> error "Formato de linha inválido"


impruc :: IO String
impruc = readFile "ucs.txt"


apreuc :: IO [(Int, Int, String)]
apreuc = do
    texto <- impruc
    let list = lines texto
        disciplinas = toDisciplinas list
    return disciplinas



toalunos :: [String] -> [(String, Int, String)]
toalunos = mapMaybe toaluno


toaluno :: String -> Maybe (String, Int, String)
toaluno str = case words str of
                     (n1:n2:rest) -> Just (n1, read n2, unwords rest)
                     _ -> Nothing


imprlisal :: IO String
imprlisal = readFile "listaalunos.txt"


apreal :: IO [(String, Int, String)]
apreal = do
    texto <- imprlisal
    let list = lines texto 
        alunos = toalunos list
    return alunos

toinscri :: [String] -> [(String, Int)]
toinscri = mapMaybe toinscr


toinscr :: String -> Maybe (String, Int)
toinscr str = case words str of
                     (n1:n2:rest) -> Just (n1, read [head n2])
                     _ -> Nothing


impins :: IO String
impins = readFile "inscricoes.txt"


apreins :: IO [(String, Int)]
apreins = do
    texto <- impins 
    let list = lines texto
        inscricao = toinscri list
    return inscricao

extrairUCSeAno :: [(Int, Int, String)] -> [(String, Int)]
extrairUCSeAno = map (\(_, ano, nome) -> (nome, ano))

extrairUCS :: [(Int, Int, String)] -> [String]
extrairUCS = map (\(_, _, nome) -> nome)

-------------------------------------------------------------------------------------------------------------------------------------------
----- PARTE 2 ------------------------------------------------------------------------------------------
----- FUNCOES DA ALINEA 1 e 2 --------------------------------------------------------------------------

data Exame = Exame { uc :: String, sala :: String, dia :: String, ano :: Int } deriving (Show, Eq, Ord)

enumerarsalas :: Int -> [String]
enumerarsalas n = map (\i -> "Sala" ++ show i) [1..n]

enumerardias :: Int -> [String]
enumerardias n = map (\i -> "Dia" ++ show i) [1..n]

gerarEscalonamento :: [(String, Int)] -> [(String, String)] -> [Exame]
gerarEscalonamento ucs slots = zipWith (\uc slot -> Exame (fst uc) (fst slot) (snd slot) (snd uc)) ucs slots

sameDay :: Exame -> Exame -> Bool
sameDay exame1 exame2 = ano exame1 == ano exame2 && dia exame1 == dia exame2

verificarConflitos :: [Exame] -> Bool
verificarConflitos exames = any (\exame -> length (filter (sameDay exame) exames) > 1) exames

ponto1 :: [(Int, Int, String)] -> Int -> Int -> IO ()
ponto1 a b c = do
  let ucseano = extrairUCSeAno a
  let salas = enumerarsalas b
  let dias = enumerardias c
  let slots = [(sala, dia) | sala <- salas, dia <- dias]
  
  if length slots < length ucseano
    then putStrLn "Número insuficiente de salas e/ou dias disponíveis para acomodar todos os exames."
    else do
        let escalonamento = gerarEscalonamento ucseano slots
        if verificarConflitos escalonamento
          then putStrLn "Há conflitos no escalonamento dos exames."
          else writeFile "escalonamento.txt" (unlines $ map show escalonamento)




-------------------------FUNCOES PRINCIPAIS------------------------------------------------------------
recetor :: Int -> Int -> Int -> IO()
recetor d s l = do
    inscricoes <- apreins
    listalunos <- apreal
    cadeiras <- apreuc
    putStrLn ("Indique a opção que prefere\n\t1->Criar ficheiro para Escalonamento\n\t2->Apresentação de incompatibilidades\t")
    op <- opcoes
    if op == 1 
        then ponto1 cadeiras s d
        else if op ==2
            then return()
            else return()


menu :: IO()
menu = do
    putStrLn ("******************MENU**********************")
    putStrLn ("\n->INTRODUZA O NUMERO DE DIAS EM QUE VAO OCORRER OS EXAMES\n")
    dias <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE SALAS EXISTENTES\n")
    salas <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE LUGARES DISPONIVEIS EM CADA SALA\n")
    lugares <- opcoes
    recetor dias salas lugares
    return ()

main2 :: IO ()
main2 = do
    pause 
    menu
    return ()

main :: IO()
main = do
    apresentatrab
    pause
    funcaotrabalho
    main2
    return ()
