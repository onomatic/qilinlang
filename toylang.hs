

import Text.ParserCombinators.Parsec
import Control.Monad
import System.Environment
import qualified Data.Map as M


data QilinProgram = Program [QilinLang]
                       deriving (Show)


data FunDef =  FunDef String [QilinVal]
               deriving (Show)

data QilinLang = Val QilinVal
               | Decl FunDef 
                  deriving (Show)

data QilinVal = Prim QilinPrim | FunVal QilinFun | List [QilinVal]
              | Parens [QilinVal]
                deriving (Show)

data QilinPrim =  Number Integer
                | String String
                | Var String
                | FunVar String 
                deriving (Show)

data QilinFun = PrimFun (M.Map String QilinVal -> QilinVal -> Maybe QilinVal)
                  | Fun [QilinVal]

instance Show QilinFun where
   show (PrimFun f) = "PrimFun"
   show (Fun xs) = "Fun " ++ concatMap show xs  

qilinPlus :: M.Map String QilinVal -> QilinVal -> Maybe QilinVal
qilinPlus fs qv = do v <- eval fs qv
                     return $ FunVal $ PrimFun $ (\fs' qv' -> eval fs' qv' >>= qilinAdd v)
          where qilinAdd (Prim (Number x)) (Prim (Number y)) = Just $ Prim $ Number $ x + y
                qilinAdd (Prim (String _)) _ = Nothing
                qilinAdd x y = do x' <- eval fs x
                                  y' <- eval fs y
                                  qilinAdd x' y'

{-
qilinEval :: M.Map String QilinVal -> QilinVal -> Maybe QilinVal
qilinEval fs (Prim (Number x)) = Nothing
qilinEval fs (Prim (String s)) = return $ FunVal $ PrimFun $ const $ (\fs' qv' -> eatQilin' fs f >>= flip (apply fs') qv')
        where f = parseInput s
              eatQilin' = eatQilin funs $ map getVals' $ filter getVals xs $ parseInput s 
-}


qilinK :: QilinVal -> Maybe QilinVal
qilinK v = Just $ FunVal $ PrimFun $ const (\x -> Just v)

qilinS :: QilinVal -> Maybe QilinVal
qilinS x = Just $ FunVal $ PrimFun $ const (\y -> Just $ FunVal $ PrimFun $ const (\z -> Just $ FunVal $ Fun $ [x, z, (Parens [y, z])]))


qilinI :: QilinVal -> Maybe QilinVal
qilinI v = Just v


qompQilin (Program xs) = let funs = foldl stuffInto opMap $ map getDecs' $ filter getDecs xs
                         in do val <- eatQilin funs $ map getVals' $ filter getVals xs 
                               eval funs val

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
opMap = M.insert "S" (FunVal $ PrimFun $ const qilinS) $ 
        M.insert "I" (FunVal $ PrimFun $ const qilinI) $ 
        M.insert "K" (FunVal $ PrimFun $ const qilinK) $
        M.insert "+"  (FunVal $ PrimFun qilinPlus) M.empty

eval ::  M.Map String QilinVal -> QilinVal -> Maybe QilinVal
eval fs (Parens xs) = eatQilin fs xs
eval fs (Prim (FunVar s)) = M.lookup s fs
eval fs (FunVal (Fun xs)) = eatQilin fs xs
eval fs x = Just x

apply ::  M.Map String QilinVal -> QilinVal -> QilinVal -> Maybe QilinVal
apply fs (FunVal (PrimFun f)) x = f fs x
apply fs (Parens xs) x = eatQilin fs xs >>= flip (apply fs) x
apply fs (FunVal (Fun xs)) x = eatQilin fs >=> flip (apply fs) x $ xs
apply fs (Prim (FunVar s)) x = do f <- M.lookup s fs
                                  apply fs f x

apply fs f (Parens xs) = (apply fs f) =<< eatQilin fs xs
apply fs f (Prim (FunVar s)) = do x <- M.lookup s fs
                                  apply fs f x
apply _ f x = error $ show f ++ " " ++ show x

parseNumber :: Parser QilinPrim
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser QilinPrim
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseVar :: Parser QilinPrim
parseVar = liftM (Var) $ many1 lower

symbol :: Parser Char
symbol = oneOf "~`!@#$%^&*_-+={}|\\:;<>,.?/"


parseQilinPrim :: Parser QilinPrim
parseQilinPrim = parseNumber <|> parseString <|> parseVar <|> parseQilinFunVar

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

parseQilinVal :: Parser QilinVal
parseQilinVal = parseQilinPrimVal <|> parseQilinListVal <|> parseQilinParensVal

parseQilinFunVar :: Parser QilinPrim
parseQilinFunVar = liftM (FunVar) $ many1 symbol <|> (liftM return upper)


parseQilinLang :: Parser [QilinLang]
parseQilinLang =  (liftM (:[]) parseQilinLangDef <|> parseQilinLangVal)  

parseInput input = parse parseQilinProgram "Qilin" input 

runProgram (Left err)  = "No match: " ++ show err
runProgram (Right ast) = (show ast) ++ "\nEvaluates To:\n" ++ (show $ qompQilin ast) ++ "\n"

main = forever $ do xs <- getLine
                    putStr $ runProgram . parseInput $ xs


