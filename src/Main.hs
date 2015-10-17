{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, StandaloneDeriving #-}
module Main where
import Control.Monad
import Text.Parsec

data Identifier = Id String
data LogicVar = Lit Identifier | Not Identifier

data Formula = Var LogicVar | And Formula Formula | Or Formula Formula | Negate Formula

data CNFClause = OrNode [LogicVar]
data CNF = AndNode [CNFClause]

deriving instance Show Identifier
deriving instance Show LogicVar
deriving instance Show Formula
deriving instance Show CNFClause
deriving instance Show CNF

whitespace = many space

parseNot = char '!' <|> char '~'
parseAlpha = oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
parseIdentifier = Id <$> many1 parseAlpha
parseLogicVar = (Lit <$> parseIdentifier) <|> (Not <$> (parseNot *> whitespace *> parseIdentifier))

{-
parseFormula = parseAnd <|> parseOr <|> parseNegate <|> (Var <$> parseLogicVar)
parseBinop ctor sym = ctor <$> parseFormula <*> (whitespace <* string sym *> whitespace *> parseFormula)
parseAnd = parseBinop And "&&"
parseOr = parseBinop Or "||"
parseNegate = Negate <$> (parseNot *> whitespace *> parseFormula)
-}

parseCNFClause = OrNode <$> (parseLogicVar `sepBy1` (whitespace *> string "||" <* whitespace))
parseCNF = AndNode <$> ((char '(' *> parseCNFClause <* char ')') `sepBy1` (whitespace *> string "&&" <* whitespace))

main = forever (getLine >>= print . parse parseCNF "")
