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
    putStrLn "Planificacao para a gestao de salas e exmes em Haskell"
    putStrLn "  "
    putStrLn "Realizado por:"
    putStrLn "   *Carolina Machado al79359"
    putStrLn "   *David Fidalgo al79881"
    putStrLn "   *Joao Fraga al78400"
    return ()

-- Função que recebe uma lista de strings representando as linhas do arquivo e retorna uma lista de tuplas com os números e nome das disciplinas
toDisciplinas :: [String] -> [(Int, Int, String)]
toDisciplinas = map toDisciplina

-- Função que recebe uma string representando uma linha do arquivo e retorna uma tupla com o número e nome da disciplina
toDisciplina :: String -> (Int, Int, String)
toDisciplina str = case words str of
                     (n1:n2:rest) -> (read n1, read n2, unwords rest)
                     _ -> error "Formato de linha inválido"

-- Função que lê o arquivo e retorna uma string com o seu conteúdo
impruc :: IO String
impruc = readFile "ucs.txt"

-- Função secundária que lê ficheiro das ucs e apresenta as em lista
apreuc :: IO [(Int, Int, String)]
apreuc = do
    texto <- impruc
    let list = lines texto
        disciplinas = toDisciplinas list
    return disciplinas

-----------------------------------------------------------------------------------------

toalunos :: [String] -> [(String, Int, String)]
toalunos = mapMaybe toaluno

-- Função que recebe uma string representando uma linha do arquivo e retorna uma tupla com o código, número e nome da disciplina
toaluno :: String -> Maybe (String, Int, String)
toaluno str = case words str of
                     (n1:n2:rest) -> Just (n1, read n2, unwords rest)
                     _ -> Nothing



-- Função que retorna o que está escrito no ficheiro listaalunos.txt
imprlisal :: IO String
imprlisal = readFile "listaalunos.txt"

-- Função secundária que lê ficheiro da lista de alunos e apresenta as em lista
apreal :: IO [(String, Int, String)]
apreal = do
    texto <- imprlisal
    let list = lines texto 
        alunos = toalunos list
    return alunos
---------------------------------------------------------------------------------
-- Função que recebe uma string e retorna uma lista de truplas
toinscri :: [String] -> [(String, Int)]
toinscri = mapMaybe toinscr

-- Função que recebe uma string representando uma linha do arquivo e retorna uma tupla com o código de aluno e numero da disciplina
toinscr :: String -> Maybe (String, Int)
toinscr str = case words str of
                     (n1:n2:rest) -> Just (n1, read [head n2])
                     _ -> Nothing

-- Função que vai buscar oq está no ficheiro inscrições.txt
impins :: IO String
impins = readFile "inscricoes.txt"

-- Função secundária das inscrições
apreins :: IO [(String, Int)]
apreins = do
    texto <- impins 
    let list = lines texto
        inscricao = toinscri list
    return inscricao

------------------------------------------------------------------------------

-- Função que apresenta todas as cadeiras e os respetivos alunos que estão inscritos
encdisc :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> [(String, [(String, Int)])]
encdisc disciplinas alunos inscricoes =
    let resultado = encdisci alunos inscricoes <$> disciplinas
    in resultado
  where
    encdisci :: [(String, Int)] -> [(String, Int, String)] -> (Int, Int, String) -> (String, [(String, Int)])
    encdisci alunos inscricoes (cod, _, disc) = (disc, alunosinscritos)
      where
        alunosinscritos = filter (\(_, c) -> c == cod) alunos


-- Função que apresenta todos os alunos e as respetivas cadeiras que estão inscritos
encal :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> IO ()
encal disciplinas alunos inscricoes = do
    let resultado = mapaluno disciplinas inscricoes <$> nub (map fst alunos)
    mapM_ putStrLn resultado
  where
    mapaluno :: [(Int, Int, String)] -> [(String, Int, String)] -> String -> String
    mapaluno disciplinas inscricoes aluno = aluno ++ ":\n" ++ discal
      where
        discal = unlines $ map (getDisciplinaNome disciplinas) disciplinasins        
        disciplinasins = map snd $ filter (\(a, _) -> a == aluno) alunos
        getDisciplinaNome :: [(Int, Int, String)] -> Int -> String
        getDisciplinaNome disciplinas codigo = head [disc | (c, _, disc) <- disciplinas, c == codigo]

----------------------------------------------------------------
visualizarcadeira :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> String -> IO ()
visualizarcadeira disciplinas alunos inscricoes nome = do
    let alunosnacadeira = getalunosnacadeira disciplinas alunos inscricoes nome
    putStrLn (nome ++ ":")
    if null alunosnacadeira
        then putStrLn "CADEIRA NAO ENCONTRADA"
        else mapM_ putStrLn alunosnacadeira

getalunosnacadeira :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> String -> [String]
getalunosnacadeira disciplinas alunos inscricoes nomeCadeira = [aluno | (aluno, codigo) <- alunos, (codigo', _, disciplina) <- disciplinas, codigo == codigo', disciplina == nomeCadeira]

------------------------------------------------------
visualizaraluno :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> String -> IO ()
visualizaraluno disciplinas alunos inscricoes codigoaluno = do
    let disciplinasDoAluno = getDisciplinasDoAluno disciplinas alunos inscricoes codigoaluno
    putStrLn ("Disciplinas do aluno " ++ codigoaluno ++ ":")
    if null disciplinasDoAluno
        then putStrLn "NAO ENCONTRADO"
        else mapM_ putStrLn disciplinasDoAluno

getDisciplinasDoAluno :: [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> String -> [String]
getDisciplinasDoAluno disciplinas alunos inscricoes codigoAluno = [disciplina | (aluno, codigo) <- alunos, (codigo', _, disciplina) <- disciplinas, codigo == codigo', aluno == codigoAluno]
------------------------------------------------------------------------------------------
verficheiros :: IO()
verficheiros = do
    uc <- impruc 
    ins <- imprlisal
    al <- impins 
    print uc 
    putStrLn "***********************************************************"
    print ins 
    putStrLn "***********************************************************"
    print al
    return ()
----------------------------------------------------------------------------------------------
chamaselecop ::  Int -> [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)]-> IO()
chamaselecop x cadeiras alunos inscricoes
    |x==1 = do
        putStrLn "Qual o nome da cadeira?"
        uc <- getLine
        visualizarcadeira cadeiras alunos inscricoes uc
        pause
        main2 
        
    |x==2 = do
        putStrLn "Qual o numero do aluno?(alxxx)"
        al <- getLine
        visualizaraluno cadeiras alunos inscricoes al
        main2
    |x==0 = do
        main2
    |otherwise = do 
        putStrLn "OPCAO INVALIDA"
        pause
        main2
       

opcoes :: IO Int
opcoes = do
    op <- getLine
    let ope = read op :: Int
    return ope

execop :: Int -> [(Int, Int, String)] -> [(String, Int)] -> [(String, Int, String)] -> IO()
execop x uc ins al
    |x==1 = do
        putStrLn (encdisc uc ins al)
        main2
    |x==2 = do
        encal uc ins al
        main2
    |x==3 = do
        putStrLn "Qual quer visualizar?\n1->Ver UC\n2->VER Aluno"
        op <- opcoes
        chamaselecop op uc ins al
    |x==4 = do
        verficheiros
        main2
    |x==0 = do
        return ()

----Função que recebe as funções dos ficheiros em forma de lista e dirige para as opções

receitfic :: IO ()
receitfic = do
    cadeiras <- apreuc
    inscricoes <- apreins
    alunos <- apreal
    op<- opcoes
    execop op cadeiras inscricoes alunos
    return ()



menu :: IO()
menu = do
    putStrLn ("******************MENU**********************")
    putStrLn ("\n1->Ver UC \n2->Ver Alunos \n3->Filtrar por UC/Alunos\n4->Ver Ficheiros\n0->Sair \n    Qual a opção?:")
    receitfic
    return ()

main2 :: IO ()
main2 = do
    pause 
    menu
    return ()

main :: IO()
main = do
    apresentatrab 
    main2
    return ()








