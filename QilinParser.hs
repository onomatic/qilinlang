module QilinParser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M


type TypeName = String


type QilinMap = M.Map String QilinVal

data QilinProgram = Program [QilinLang]
                       deriving (Show)


data FunDef =  FunDef String [QilinVal]
               deriving (Show)

data QilinLang = Val QilinVal
               | Decl FunDef 
                 deriving (Show)


data QilinDataType = DataType TypeName [QilinFun]
                   deriving (Show)

data QilinType = Type TypeName | FunType QilinType QilinType

data QilinVal = Prim QilinPrim | FunVal QilinFun | List [QilinVal]
              | Parens [QilinVal] | Quote [QilinVal]
                deriving (Show)

data QilinPrim =  Number Integer
                | Str String
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
                 return $ Str x

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


parseQilinDataType :: Parser QilinDataType
parseQilinDataType = do string "Data"
                        space
                        u <- upper
                        name <- many1 lower
                        return $ DataType (u:name) []

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