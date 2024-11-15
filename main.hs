import Data.Char (isAlpha, isUpper, isLower, isAlphaNum)

-- Parte 1
data Term
    = Var String
    | Const String
    | Fun String [Term]
    deriving (Eq, Show)

-- Definição de tokens para análise léxica
data Token = TokVar String | TokConst String | TokFun String | TokLParen | TokRParen | TokComma
    deriving (Eq, Show)

-- Tokenização
tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c:cs)
    | c == '('  = (TokLParen :) <$> tokenize cs
    | c == ')'  = (TokRParen :) <$> tokenize cs
    | c == ','  = (TokComma :) <$> tokenize cs
    | isUpper c = let (name, rest) = span isAlpha cs in (TokConst (c:name) :) <$> tokenize rest
    | isLower c = let (name, rest) = span isAlphaNum cs in (TokVar (c:name) :) <$> tokenize rest
    | otherwise = Left $ "Caractere inesperado: " ++ [c]

-- Parser para converter tokens em um termo
parseTerm :: [Token] -> Either String (Term, [Token])
parseTerm (TokVar v : TokLParen : tokens) = do
    (args, rest) <- parseArgs tokens
    Right (Fun v args, rest)
parseTerm (TokConst c : TokLParen : tokens) = do
    (args, rest) <- parseArgs tokens
    Right (Fun c args, rest)
parseTerm (TokVar v : tokens) = Right (Var v, tokens)
parseTerm (TokConst c : tokens) = Right (Const c, tokens)
parseTerm [] = Left "Erro de parsing: Termo vazio."
parseTerm tokens = Left $ "Erro de parsing: tokens inesperados " ++ show tokens

-- Parsing dos argumentos da função ou predicado
parseArgs :: [Token] -> Either String ([Term], [Token])
parseArgs (TokRParen : tokens) = Right ([], tokens)
parseArgs tokens = do
    (arg, tokens1) <- parseTerm tokens
    case tokens1 of
        TokComma : tokens2 -> do
            (args, rest) <- parseArgs tokens2
            Right (arg : args, rest)
        TokRParen : tokens2 -> Right ([arg], tokens2)
        _ -> Left "Erro de parsing: esperado ',' ou ')'."

-- Função para unificar dois termos com verificação de ocorrências
unificar :: Term -> Term -> Either String [(String, Term)]
unificar (Var x) t2 = unificarVar x t2 []
unificar t1 (Var y) = unificarVar y t1 []
unificar (Const c1) (Const c2)
    | c1 == c2 = Right []
    | otherwise = Left $ "Constantes diferentes: " ++ c1 ++ " e " ++ c2
unificar (Fun f1 args1) (Fun f2 args2)
    | f1 == f2  = unificarArgs args1 args2 []
    | otherwise = Left $ "Funções diferentes: " ++ f1 ++ " e " ++ f2

type Substituicao = [(String, Term)]

-- Função auxiliar para unificar variáveis
unificarVar :: String -> Term -> Substituicao -> Either String Substituicao
unificarVar x t subst
    | t == Var x = Right subst
    | verificaOcorrencia x t = Left $ "Falha na verificação de ocorrências: " ++ x ++ " ocorre no termo " ++ show t
    | otherwise = Right $ (x, aplicarSubstRec subst t) : [(x, t)]

-- Verifica se a variável ocorre no termo
verificaOcorrencia :: String -> Term -> Bool
verificaOcorrencia x (Var v)     = x == v
verificaOcorrencia x (Const _)   = False
verificaOcorrencia x (Fun _ ts)  = any (verificaOcorrencia x) ts

-- Unifica listas de argumentos e propaga as substituições
unificarArgs :: [Term] -> [Term] -> Substituicao -> Either String Substituicao
unificarArgs [] [] subst = Right subst
unificarArgs (t1:ts1) (t2:ts2) subst = do
    subst' <- unificar (aplicarSubstRec subst t1) (aplicarSubstRec subst t2)
    let substTotal = subst' ++ subst
    unificarArgs ts1 ts2 substTotal
unificarArgs _ _ _ = Left "Listas de argumentos com tamanhos diferentes."

-- Aplica uma lista de substituições a um termo de forma recursiva até atingir um ponto fixo
aplicarSubstRec :: Substituicao -> Term -> Term
aplicarSubstRec [] t = t
aplicarSubstRec ((x, s):subst) t@(Var v)
    | x == v    = aplicarSubstRec subst s
    | otherwise = aplicarSubstRec subst t
aplicarSubstRec subst (Fun f args) = Fun f (map (aplicarSubstRec subst) args)
aplicarSubstRec _ t = t

-- Função principal para teste
main :: IO ()
main = do
    putStrLn "Insira o primeiro termo da lógica de primeira ordem:"
    input1 <- getLine
    putStrLn "Insira o segundo termo da lógica de primeira ordem:"
    input2 <- getLine

    let term1Tokens = tokenize input1
    let term2Tokens = tokenize input2

    case (term1Tokens, term2Tokens) of
        (Right tokens1, Right tokens2) -> do
            let term1 = parseTerm tokens1
            let term2 = parseTerm tokens2
            case (term1, term2) of
                (Right (t1, _), Right (t2, _)) -> do
                    let result = unificar t1 t2
                    case result of
                        Right subst -> putStrLn $ "Os termos são unificáveis\nSubstituições: " ++ show subst
                        Left err    -> putStrLn $ "Erro de unificação: " ++ err
                _ -> putStrLn "Erro ao interpretar os termos."
        _ -> putStrLn "Erro de sintaxe: Entrada inválida ou caractere inesperado."
