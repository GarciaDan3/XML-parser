{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, MultiParamTypeClasses #-}
module XMLparser where

import Control.Applicative (Alternative(..), many)
import Control.Monad (guard, when)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)

data XML = Text String | Element String [XML] deriving (Show, Eq)

newtype Parser a = Parser { runParser :: StateT String Maybe a } 
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadState String)

satisfy f = Parser $ do (x:xs) <- get; guard (f x); put xs; pure x

spaces = many (satisfy (`elem` " \t\n\r"))

notCloseTag t = Parser $ do s <- get; when (take (length tag) s == tag) empty where tag = "</" ++ t ++ ">"

parseText = do c <- many (satisfy (/= '<')); guard (not (null c)); pure (Text c)

parseElement = do
    _ <- satisfy (== '<'); name <- many (satisfy (`notElem` "> ")); _ <- spaces; _ <- satisfy (== '>')
    children <- many (notCloseTag name *> (parseElement <|> parseText))
    mapM_ (satisfy . (==)) ("</" ++ name ++ ">")
    pure (Element name children)