import System.IO 
import Data.Char 
import Text.Read 
import Data.Maybe 
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
    putStrLn "Este trabalho aborda a gestão e o escalonamento de horários e salas para a realização de\n exames, no âmbito de uma instituição de ensino superior.\n Na primeira fase do trabalho foi nos solicitado o desenvolvimento de soluções que permitam a\n gestão e visualização da informação disponível. \nEsta informação é relativa aos alunos, às unidades curriculares consideradas e à inscrição\n de alunos nas diversas unidades curriculares.\n Nesta segunda fase do trabalho é nos solicitado o \ndesenvolvimento de soluções que permitam a proposta do escalonamento de horários e \nsalas para a realização dos exames das diversas unidades curriculares.\n\n"
    return ()

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------ Funcoes Trabalho Projeto I ------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

encdisc :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> IO [(String, [String])]
encdisc disciplinas alunos inscricoes = do
    let resultado = map (encdisci alunos inscricoes) disciplinas
    return resultado
  where
    encdisci :: [(String, Int)] -> [(String, Int, String)] -> (Int, Int, String) -> (String, [String])
    encdisci alunos inscricoes (cod, _, disc) = (disc, [aluno | (aluno, codAluno) <- alunos, codAluno == cod])

extrairUCSeAno :: [(Int, Int, String)] -> [(String, Int)]
extrairUCSeAno = map (\(_, ano, nome) -> (nome, ano))

extrairUCS :: [(Int, Int, String)] -> [String]
extrairUCS = map (\(_, _, nome) -> nome)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------ PARTE 2 ------------------------------------------------------------------------------------------
----- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------- OPCAO 1 DO MENU ---------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Exame = Exame { uc :: String, salas :: [String], dia :: String, ano :: Int, alunos :: [String] } deriving (Show, Eq, Ord)

enumerarsalas :: Int -> [String]
enumerarsalas n = map (\i -> "Sala" ++ show i) [1..n]

enumerardias :: Int -> [String]
enumerardias n = map (\i -> "Dia" ++ show i) [1..n]

gerarEscalonamento :: [(String, Int)] -> [(String, String)] -> [(String, [String])] -> Int -> [Exame]
gerarEscalonamento ucs slots alunoLista max_capacidade =
  let exames = zipWith (\(uc, ano) (sala, dia) -> Exame uc [sala] dia ano (fromMaybe [] (lookup uc alunoLista))) ucs slots
  in map (\exame -> exame { salas = splitStudents (salas exame) (alunos exame) }) exames
  where
    splitStudents :: [String] -> [String] -> [String]
    splitStudents salas alunos
      | length alunos > max_capacidade = splitStudents (salas ++ [nextSala (last salas)]) (drop max_capacidade alunos)
      | otherwise = salas
    nextSala :: String -> String
    nextSala sala = "Sala" ++ show (read (drop 4 sala) + 1)


igualdia :: Exame -> Exame -> Bool
igualdia exame1 exame2 = ano exame1 == ano exame2 && dia exame1 == dia exame2 && null (intersect (salas exame1) (salas exame2))

verificarConflitos :: [Exame] -> Bool
verificarConflitos exames = any (\exame -> length (filter (igualdia exame) exames) > 1) exames

getAlunosInscritos :: Exame -> [Exame] -> [String]
getAlunosInscritos exame exames = alunos $ head $ filter ((== uc exame) . uc) exames

calcIncompatibilidadesponto1 :: [Exame] -> [(String, String, Int)]
calcIncompatibilidadesponto1 exames =
  let ucs = map uc exames
      paresUCs = [(uc1, uc2) | uc1 <- ucs, uc2 <- ucs, uc1 /= uc2]
      incompatibilidades = map (\(uc1, uc2) -> 
        let exame1 = head $ filter ((== uc1) . uc) exames
            exame2 = head $ filter ((== uc2) . uc) exames
            inc = length $ intersect (alunos exame1) (alunos exame2)
        in (uc1, uc2, inc)) paresUCs
  in filter (\(_,_,inc) -> inc > 0) incompatibilidades



writeIncompatibilidades :: [Exame] -> IO ()
writeIncompatibilidades exames = do
  let incompatibilidades = calcIncompatibilidadesponto1 exames
  appendFile "escalonamento.txt" "\nIncompatibilidades entre pares de exames:\n"
  mapM_ (\(uc1, uc2, incompat) -> appendFile "escalonamento.txt" $ "UCs " ++ uc1 ++ " e " ++ uc2 ++ ": " ++ show incompat ++ " alunos inscritos em ambas\n") incompatibilidades

ponto1 :: [(Int, Int, String)] -> [(String, [String])] -> Int -> Int -> Int-> IO ()
ponto1 a alinsUC b c max_capacidade= do
  let ucseano = extrairUCSeAno a
  let salas = enumerarsalas b
  let dias = enumerardias c
  let slots = [(sala, dia) | sala <- salas, dia <- dias]

  if length slots < length ucseano
    then putStrLn "Número insuficiente de salas e/ou dias disponíveis para acomodar todos os exames existentes."
    else do
        let escalonamento = gerarEscalonamento ucseano slots alinsUC max_capacidade
        if verificarConflitos escalonamento
          then putStrLn "Há conflitos no escalonamento dos exames."
          else do
            writeFile "escalonamento.txt" (unlines $ map show escalonamento)
            writeIncompatibilidades escalonamento
            putStrLn "Visialize o ficheiro escalonamento.txt para visualizar as alteraçoes feitas!"



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------- OPCAO 2 DO MENU---- ----------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

type UC = String
type Aluno = String

getAlunosInscritosponto2 :: UC -> [(UC, [Aluno])] -> [Aluno]
getAlunosInscritosponto2 uc lst = fromMaybe [] (lookup uc lst)

calcIncompatibilidades :: [(UC, [Aluno])] -> [(UC, UC, Int)]
calcIncompatibilidades inscricoes =
  let ucs = map fst inscricoes
      paresUCs = [(uc1, uc2) | uc1 <- ucs, uc2 <- ucs, uc1 /= uc2]
      incompatibilidades = map (\(uc1, uc2) -> (uc1, uc2, length (intersect (getAlunosInscritosponto2 uc1 inscricoes) (getAlunosInscritosponto2 uc2 inscricoes)))) paresUCs
  in incompatibilidades

ponto2 :: [(String, [String])] -> IO ()
ponto2 a = do
  let incompatibilidades = calcIncompatibilidades a
  putStrLn "Incompatibilidades entre pares de UCs:"
  mapM_ (\(uc1, uc2, incompat) -> putStrLn $ "UCs " ++ uc1 ++ " e " ++ uc2 ++ ": " ++ show incompat ++ " alunos inscritos em ambas") incompatibilidades



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------- FUNCOES PRINCIPAIS --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

recetor :: Int -> Int -> Int -> IO()
recetor d s l = do
    inscricao <- apreins
    listalunos <- apreal
    cadeiras <- apreuc
    inscUCS <-encdisc cadeiras inscricao listalunos
    putStrLn ("Indique a opção que prefere\n\t1->Criar ficheiro para Escalonamento\n\t2->Apresentação de incompatibilidades\n\tQualquer opção->Sair")
    op <- opcoes
    if op == 1 
        then do
            ponto1 cadeiras inscUCS s d l
            main2
        else if op ==2
            then do
                ponto2 inscUCS
                main2
            else do
                return ()


menu :: IO()
menu = do
    putStrLn ("******************MENU**********************")
    putStrLn ("\n->INTRODUZA O NUMERO DE DIAS EM QUE VAO OCORRER OS EXAMES\n")
    dias <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE SALAS EXISTENTES\n")
    salas <- opcoes
    putStrLn ("\n->INTRODUZA O NUMERO DE LUGAWRES DISPONIVEIS EM CADA SALA\n")
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
