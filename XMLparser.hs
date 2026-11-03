{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, MultiParamTypeClasses #-}
module XMLparser where

import Control.Applicative (Alternative(..), many, some)
import Control.Monad (guard, when)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)

type Attribute = (String, String)
data XML = Text String | Element String [Attribute] [XML] deriving (Show, Eq)

newtype Parser a = Parser { runParser :: StateT String Maybe a } 
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadState String)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ do (x:xs) <- get; guard (f x); put xs; pure x

charT :: Char -> Parser Char
charT c = satisfy (== c)

string :: String -> Parser String
string = mapM charT

spaces :: Parser String
spaces = many (satisfy (`elem` " \t\n\r"))

notCloseTag :: String -> Parser ()
notCloseTag tagName = Parser $ do
    input <- get
    when (take (length tag) input == tag) empty
  where tag = "</" ++ tagName ++ ">"

parseWhile :: (Char -> Bool) -> Parser String
parseWhile = many . satisfy

parseText :: Parser XML
parseText = do
    content <- parseWhile (/= '<')
    guard (not (null content))
    pure (Text content)

parseAttribute :: Parser Attribute
parseAttribute = do
    _ <- spaces
    key <- some (satisfy (`notElem` "=>/> "))
    _ <- spaces >> charT '=' >> spaces
    quote <- satisfy (`elem` "\"'")
    val <- many (satisfy (/= quote))
    _ <- charT quote
    pure (key, val)

parseSelfClosing :: Parser Bool
parseSelfClosing = (True <$ string "/>") <|> (False <$ charT '>')

parseTag :: Parser String
parseTag = some (satisfy (`notElem` ">/ "))

parseElement :: Parser XML
parseElement = do
    _ <- charT '<'; name <- parseTag; _ <- spaces
    attrs <- many parseAttribute; _ <- spaces
    isSelfClosing <- parseSelfClosing
    if isSelfClosing
        then pure (Element name attrs [])
        else do
            children <- many (notCloseTag name *> (parseElement <|> parseText))
            _ <- string "</" >> string name; _ <- spaces; _ <- charT '>'
            pure (Element name attrs children)

skipDeclarations :: Parser ()
skipDeclarations = do
    _ <- many (spaces *> (string "<!" <|> string "<?") *> many (satisfy (/= '>')) *> charT '>' *> spaces)
    pure ()

parseXML :: Parser XML
parseXML = (spaces >> skipDeclarations >> spaces) *> parseElement

xmlDocument :: String -> Maybe XML
xmlDocument input = case runStateT (runParser parseXML) input of 
    Just (xml, []) -> Just xml
    _              -> Nothing
