module Parser where
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Data.List.Split (splitOn)
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile a = do f <- readFile a
                 let ls = lines f
                 let ds = map (parse context "Error:") (init ls)
                 let e = parse formula "Error:" (last ls)
                 return (ds,e)

reserv = "()->{}\n "

context :: Parsec String () [Formula]
context = do {fs <- many formula; return fs}

formula :: Parsec String () (Formula)
formula = (do try $ implFormula) <|> formula'

formula' :: Parsec String () (Formula)
formula' = (do try $ orFormula) <|> (do try $ andFormula) <|> formula''

formula'' :: Parsec String () (Formula)
formula'' = (do try $ notFormula) <|> singleFormula

singleFormula :: Parsec String () (Formula)
singleFormula = do char '('
                   spaces
                   f <- formula
                   spaces
                   char ')'
                   spaces
                   return f
                <|> falsum <|> proposition

implFormula = do l <- formula'
                 spaces
                 string "->"
                 spaces
                 r <- formula'
                 spaces
                 return (Impl l r)

orFormula = do l <- formula''
               spaces
               string "v"
               spaces
               r <- formula''
               spaces
               return (Or l r)

andFormula = do l <- formula''
                spaces
                string "^"
                spaces
                r <- formula''
                spaces
                return (And l r)

notFormula = do char '¬'
                spaces
                f <- singleFormula
                spaces
                return (Not f)

falsum = do {string "0"; spaces; return (Falsum)}
proposition = do {p <- many1 (noneOf reserv); spaces; return (Proposition p)}
