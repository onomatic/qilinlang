

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

data QilinFun = PrimFun (QilinVal -> Maybe QilinVal)
                  | Fun [QilinVal]

instance Show QilinFun where
   show (PrimFun f) = "PrimFun"
   show (Fun xs) = "Fun " ++ concatMap show xs  

qilinPlus :: QilinVal -> Maybe QilinVal
qilinPlus (Prim (Number x)) = Just $ FunVal $ PrimFun $ (\v -> case v of 
	                                                                      (Prim (Number y)) -> Just $ Prim $ Number $ x + y
	                                                                      _ -> Nothing)
qilinPlus (Prim (String s)) = Just $ FunVal $ PrimFun $ (\v -> case v of 
	                                                                      (Prim (String y)) -> Just $ Prim $ String $ s ++ y 
	                                                                      _ -> Nothing)
qilinPlus qv = error $ show qv


qilinI :: QilinVal -> Maybe QilinVal
qilinI v = Just $ FunVal $ PrimFun $ (\x -> Just v)


qompQilin (Program xs) = let funs = foldl stuffInto opMap $ map getDecs' $ filter getDecs xs
                         in eatQilin funs $ map getVals' $ filter getVals xs 

foldl1Safe f def []  = def
foldl1Safe f _ xs    = foldl1 f xs

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
opMap = M.insert "|" (FunVal $ PrimFun qilinI) $ M.insert "+"  (FunVal $ PrimFun qilinPlus) M.empty

apply ::  M.Map String QilinVal -> QilinVal -> QilinVal -> Maybe QilinVal
apply fs (Parens xs) x = eatQilin fs xs >>= flip (apply fs) x
apply fs f (Parens xs) = (apply fs f) =<< eatQilin fs xs
apply fs (FunVal (Fun xs)) x = eatQilin fs >=> flip (apply fs) x $ xs
apply fs (Prim (FunVar s)) x = do f <- M.lookup s fs
                                  apply fs f x
apply fs f (Prim (FunVar s)) = do x <- M.lookup s fs
                                  apply fs f x
apply fs (FunVal (PrimFun f)) x = f x
apply _ f x = error $ show f ++ " " ++ show x

parseNumber :: Parser QilinPrim
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser QilinPrim
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseVar :: Parser QilinPrim
parseVar = liftM (Var) $ many1 letter

symbol :: Parser Char
symbol = oneOf "~`!@#$%^&*_-+={}|\\:;<>,.?/"


parseQilinPrim :: Parser QilinPrim
parseQilinPrim = parseNumber <|> parseString <|> parseVar <|> parseQilinFunVar

parseQilinProgram :: Parser QilinProgram
parseQilinProgram = do program <- endBy1 parseQilinLang newline
                       skipMany newline
                       return $ Program program

parseQilinPrimVal :: Parser QilinVal
parseQilinPrimVal = liftM (Prim) $ parseQilinPrim

parseQilinLangVal :: Parser QilinLang 
parseQilinLangVal = liftM (Val) $ parseQilinVal

parseQilinLangDef :: Parser QilinLang 
parseQilinLangDef = liftM (Decl) $ parseQilinDef

parseQilinDef :: Parser FunDef
parseQilinDef = do char '#'
                   space
                   name <- many1 symbol
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
parseQilinFunVar = liftM (FunVar) $ many1 symbol


parseQilinLang :: Parser QilinLang
parseQilinLang =  (parseQilinLangDef <|> parseQilinLangVal)  

parseInput input = parse parseQilinProgram "Qilin" input 

runProgram (Left err)  = "No match: " ++ show err
runProgram (Right ast) = (show ast) ++ "\nEvaluates To:\n" ++ (show $ qompQilin ast) ++ "\n"

main = sequence . repeat . interact $ runProgram . parseInput 


