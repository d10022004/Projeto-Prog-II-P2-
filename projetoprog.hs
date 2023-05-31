module Exem where
import System.IO 
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (nub)

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
impins = readFile "inscrições.txt"


apreins :: IO [(String, Int)]
apreins = do
    texto <- impins 
    let list = lines texto
        inscricao = toinscri list
    return inscricao
-------------------------------------------------------------------------------------------------------------------------------------------
----- PARTE 2 --------------------------------------------
-- EXEMPLO DO QUE A FUNCAO APREINS APRESENTA ->

criacaosalas :: Int -> Int -> Int -> IO()
criacaosalas d s l = do
    inscricoes <- apreins
    listalunos <- apreal
    cadeiras <- apreuc
    return ()

-------------------------FUNCOES PRINCIPAIS------------------------------------------------------------
opcoes :: IO Int
opcoes = do
    op <- getLine
    let ope = read op :: Int
    return ope

menu :: IO()
menu = do
    putStrLn ("******************MENU**********************")
    putStrLn ("\n->INTRODUZA O NUMERO DE DIAS EM QUE VAO OCORRER OS EXAMES\n")
    dias <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE SALAS EXISTENTES\n")
    salas <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE LUGARES DISPONIVEIS EM CADA SALA\n")
    lugares <- opcoes
    criacaosalas dias salas lugares
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

