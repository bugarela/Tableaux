module PrefixParser where
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Data.List.Split (splitOn)
import Head

import Control.Monad.Identity (Identity)

{-# LANGUAGE NoMonomorphismRestriction #-}

parseFile' a = do f <- readFile a
                  let ls = lines f
                  let ds = map (parse context "Error:") (init ls)
                  let e = parse formula "Error:" (last ls)
                  return (ds,e)

reserv = "0-*+)\n "

context :: Parsec String () [Formula]
context = do {fs <- many formula; return fs}

formula :: Parsec String () (Formula)
formula = do {char '0'; spaces; return (Falsum)}
          <|>
          do {char '-'; spaces; f <- formula; spaces; return (Not f)}
          <|>
          do {char '*'; spaces; l <- formula; spaces; r <-formula; spaces; return (And l r)}
          <|>
          do {char '+'; spaces; l <- formula; spaces; r <-formula; spaces; return (Or l r)}
          <|>
          do {char ')'; spaces; l <- formula; spaces; r <-formula; spaces; return (Impl l r)}
          <|> proposition

proposition = do {p <- many1 (noneOf reserv); spaces; return (Proposition p)}
