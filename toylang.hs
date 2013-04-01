

import Text.ParserCombinators.Parsec
import Control.Monad
import System.Environment
import Data.List
import qualified Data.Map as M
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Fix
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import System.IO


type QilinMap = M.Map String QilinVal

data QilinProgram = Program [QilinLang]
                       deriving (Show)


data FunDef =  FunDef String [QilinVal]
               deriving (Show)

data QilinLang = Val QilinVal
               | Decl FunDef 
                  deriving (Show)

data QilinVal = Prim QilinPrim | FunVal QilinFun | List [QilinVal]
              | Parens [QilinVal] | Quote [QilinVal]
                deriving (Show)

data QilinPrim =  Number Integer
                | String String
                | Var String
                | FunVar String
                | Bool Bool 
                deriving (Show, Eq)

data QilinFun = PrimFun (QilinMap -> QilinVal -> Maybe QilinVal)
                  | Fun [QilinVal]

instance Show QilinFun where
   show (PrimFun f) = "PrimFun"
   show (Fun xs) = "Fun " ++ show xs

instance Eq QilinVal where
    (Prim a) == (Prim b) = a == b
    _ == _ = False 


qilinPlus :: M.Map String QilinVal -> QilinVal -> Maybe QilinVal
qilinPlus fs qv = do v <- eval fs qv
                     return $ FunVal $ PrimFun $ (\fs' qv' -> eval fs' qv' >>= qilinAdd v)
          where qilinAdd (Prim (Number x)) (Prim (Number y)) = Just $ Prim $ Number $ x + y
                qilinAdd (Prim (String _)) _ = Nothing
                qilinAdd x y = do x' <- eval fs x
                                  y' <- eval fs y
                                  qilinAdd x' y'

fundef = Just . FunVal . PrimFun

qilinEq :: QilinMap -> QilinVal -> Maybe QilinVal
qilinEq qmap qv = Just $ FunVal $ PrimFun $ (\qmap' qv' -> if (totalEval' qmap qv) == (totalEval' qmap' qv') 
                                                           then (Just $ Prim $ Bool True) else (Just $ Prim $ Bool False))
                 where totalEval' funs expr = head $ totalEval funs expr

qilinIf :: QilinMap -> QilinVal -> Maybe QilinVal
qilinIf qmap qv = fundef $ (\qmap' qv' -> fundef $ (\qmap'' qv'' -> if' (head $ totalEval qmap qv) qv' qv'' ))
                where if' (Prim (Bool True)) y n  = Just $ FunVal $ Fun $ [y]
                      if' (Prim (Bool False)) y n = Just $ FunVal $ Fun $ [n]
                      if' _ _ _  = undefined

qilinK :: QilinVal -> Maybe QilinVal
qilinK v = Just $ FunVal $ PrimFun $ const (\x -> Just v)

qilinS :: QilinVal -> Maybe QilinVal
qilinS x = Just $ FunVal $ PrimFun $ const (\y -> Just $ FunVal $ PrimFun $ const (\z -> Just $ FunVal $ Fun $ [x, z, (Parens [y, z])]))


qilinI :: QilinVal -> Maybe QilinVal
qilinI v = Just v


qompQilin (Program xs) = let funs = foldl stuffInto opMap $ map getDecs' $ filter getDecs xs
                         in do val <- eatQilin funs $ map getVals' $ filter getVals xs 
                               return $ totalEval funs val

qompQilinRepl opMap (Program xs) = let funs = foldl stuffInto opMap $ map getDecs' $ filter getDecs xs
                                       val  = do expr <- eatQilin funs $ map getVals' $ filter getVals xs
                                                 return $ head $ totalEval funs expr  
                                   in (val, funs)

stuffInto :: M.Map String QilinVal -> FunDef -> M.Map String QilinVal
stuffInto mp (FunDef name xs) = M.insert name (FunVal $ Fun xs) mp 

getVals' :: QilinLang -> QilinVal
getVals' (Val v) = v
getVals' _ = undefined

getVals :: QilinLang -> Bool
getVals (Val v) = True
getVals _ = False

getDecs :: QilinLang -> Bool
getDecs (Decl d) = True
getDecs _ = False

getDecs' :: QilinLang -> FunDef
getDecs' (Decl d) = d
getDecs' _ = undefined


eatQilin :: M.Map String QilinVal -> [QilinVal] -> Maybe QilinVal
eatQilin fs (x:xs) = foldM (apply fs) x xs
eatQilin _ [] = Nothing


opMap :: M.Map String QilinVal
opMap = M.insert "?" (FunVal $ PrimFun $      qilinIf) $
        M.insert "=" (FunVal $ PrimFun $      qilinEq) $
        M.insert "S" (FunVal $ PrimFun $ const qilinS) $ 
        M.insert "I" (FunVal $ PrimFun $ const qilinI) $ 
        M.insert "K" (FunVal $ PrimFun $ const qilinK) $
        M.insert "+"  (FunVal $ PrimFun qilinPlus) M.empty

eval' ::  M.Map String QilinVal -> QilinVal -> Maybe QilinVal
eval' fs (Parens xs) = eatQilin fs xs
eval' fs (Prim (FunVar s)) = M.lookup s fs
eval' fs (FunVal (Fun xs)) = eatQilin fs xs
eval' fs x = Nothing

w f x = f x x 

totalEval :: QilinMap -> QilinVal -> [QilinVal]
totalEval = (<*> return) . ((++) .) . unfoldr . (fmap (w (,)) .) . eval' 

eval ::  M.Map String QilinVal -> QilinVal -> Maybe QilinVal
eval fs (Parens xs) = eatQilin fs xs
eval fs (Prim (FunVar s)) = M.lookup s fs
eval fs (FunVal (Fun xs)) = eatQilin fs xs
eval fs x = Just x

foldList :: [QilinVal] -> QilinVal -> QilinVal
foldList [] f  = (Prim (FunVar "I"))
foldList [x] f = Parens [f,x]
foldList (x:xs) f =  Parens [(Prim (FunVar "B")), Parens [f,x], Parens [foldList xs f]] 

apply ::  M.Map String QilinVal -> QilinVal -> QilinVal -> Maybe QilinVal
apply fs (Quote xs) f = eatQilin fs (xs ++ [f])
apply fs f (Prim (FunVar s)) = do x <- M.lookup s fs
                                  apply fs f x
apply fs f (Quote xs) = eatQilin fs (f:xs)
apply fs (List xs) x = Just $ foldList xs x
apply fs (FunVal (PrimFun f)) x = f fs x
apply fs (Parens xs) x = eatQilin fs xs >>= flip (apply fs) x
apply fs (FunVal (Fun xs)) x = eatQilin fs >=> flip (apply fs) x $ xs
apply fs (Prim (FunVar s)) x = do f <- M.lookup s fs
                                  apply fs f x

apply fs f (Parens xs) = (apply fs f) =<< eatQilin fs xs
apply _ f x = error $ show f ++ " " ++ show x

parseNumber :: Parser QilinPrim
parseNumber = liftM (Number . read) $ many1 digit

parseBool :: Parser QilinPrim
parseBool = liftM (Bool . readBool) $ oneOf "tf"
           where readBool 't' = True
                 readBool 'f' = False

parseString :: Parser QilinPrim
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseVar :: Parser QilinPrim
parseVar = liftM (Var) $ many1 lower

symbol :: Parser Char
symbol = oneOf "~`!@#$%^&*_-+=|\\:;<>,.?/"


parseQilinPrim :: Parser QilinPrim
parseQilinPrim = parseNumber <|> parseString <|> parseBool <|> parseVar <|> parseQilinFunVar

parseQilinProgram :: Parser QilinProgram
parseQilinProgram = do program <- sepEndBy1 parseQilinLang newline
                       skipMany newline
                       return $ Program $ concat program

parseQilinPrimVal :: Parser QilinVal
parseQilinPrimVal = liftM (Prim) $ parseQilinPrim

parseQilinLangVal :: Parser [QilinLang]
parseQilinLangVal = do vs <- sepBy parseQilinVal spaces
                       return $ map (Val) vs 

parseQilinLangDef :: Parser QilinLang 
parseQilinLangDef = liftM (Decl) $ parseQilinDef

parseQilinDef :: Parser FunDef
parseQilinDef = do char '#'
                   space
                   name <- many1 symbol <|> (liftM return upper)
                   space
                   char '('  
                   xs <- sepBy parseQilinVal spaces
                   char ')'
                   return $ FunDef name xs


parseQilinListVal :: Parser QilinVal
parseQilinListVal = liftM (List) $ do char '[' 
                                      x <- sepBy parseQilinVal (spaces >> char ',' >> spaces) 
                                      char ']'
                                      return x



parseQilinParensVal :: Parser QilinVal
parseQilinParensVal = liftM (Parens) $ do char '(' 
                                          x <- sepBy parseQilinVal spaces
                                          char ')'
                                          return x

parseQilinQuoteVal :: Parser QilinVal
parseQilinQuoteVal = liftM (Quote) $ do char '{' 
                                        x <- sepBy parseQilinVal spaces
                                        char '}'
                                        return x

parseQilinVal :: Parser QilinVal
parseQilinVal = parseQilinPrimVal <|> parseQilinListVal <|> parseQilinParensVal <|> parseQilinQuoteVal

parseQilinFunVar :: Parser QilinPrim
parseQilinFunVar = liftM (FunVar) $ many1 symbol <|> (liftM return upper)


parseQilinLang :: Parser [QilinLang]
parseQilinLang =  (liftM (:[]) parseQilinLangDef <|> parseQilinLangVal)  

parseInput input = parse parseQilinProgram "Qilin" input 

fstAp = (<*> snd) . (. fst) . ((,) .)

runProgram  _   (Left err)  =  ("No match: " ++ show err, M.empty)
runProgram funs (Right ast) =  fstAp printAst $ qompQilinRepl funs ast 
                          where printAst x = (show ast) ++ "\nEvaluates To:\n" ++ (show $ x) ++ "\n"

runAndPrintProgram funs xs = let (ast, funs') = (runProgram funs) . parseInput $ xs
                             in do putStr ast
                                   return funs'

eitherToMaybe :: (a -> Either b c) -> a -> (Maybe c)
eitherToMaybe f a = case (f a) of 
                       (Left err) -> Nothing
                       (Right rv) -> Just rv

getFuns :: Handle -> IO (Maybe QilinMap)
getFuns handle = runMaybeT $ do lift $ putStrLn ("Loading file : " ++ show handle) >> hFlush stdout
                                xs  <- lift $ hGetContents handle
                                (Program ast) <- (MaybeT . return ) $ eitherToMaybe parseInput xs
                                return $ foldl stuffInto opMap $ map getDecs' $ filter getDecs ast


loadLib :: QilinMap -> String -> MaybeT IO QilinMap
loadLib qmap xs = let filenames = tail $ words xs
                  in do xs <- lift $ mapM (flip (flip withFile ReadMode) getFuns) filenames
                        MaybeT . return $ foldr ((<*>) . (M.union <$>)) (Just M.empty) xs



repl :: QilinMap -> MaybeT IO QilinMap
repl funs = do lift $ putStr "pixiu>>>" >> hFlush stdout
               xs <- lift getLine
               guard (xs /= "quit")
               funs' <- if and (zipWith (==) "load " xs) then loadLib funs xs else lift $ runAndPrintProgram funs xs
               return (funs `M.union` funs')

mrepeat f a = mrepeat f =<< f a

main = runMaybeT $ mrepeat repl opMap 


