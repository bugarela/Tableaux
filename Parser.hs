module Parser where
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile a = do f <- readFile a
                 let ls = lines f
                 let ds = map (parse context "Error:") (init ls)
                 let e = parse formula "Error:" (last ls)
                 return (ds,e)

reserv = "()->~{}\n "

context :: Parsec String () [Formula]
context = do {fs <- many formula; return fs}

formula = buildExpressionParser logicOperators singleFormula

logicOperators :: [[Operator String u Identity Formula]]
logicOperators = [[Prefix (char '~' >> spaces >> return (Not))],

                  [Infix  (char '^' >> spaces >> return (And)) AssocLeft,
                   Infix (char 'v' >> spaces >> return (Or)) AssocLeft],

                  [Infix (string "->" >> spaces >> return (Impl)) AssocLeft]]

singleFormula :: Parsec String () (Formula)
singleFormula = do char '('
                   spaces
                   f <- formula
                   spaces
                   char ')'
                   spaces
                   return f
                <|> falsum <|> proposition

falsum = do {string "0"; spaces; return (Falsum)}
proposition = do {p <- many1 (noneOf reserv); spaces; return (Proposition p)}
