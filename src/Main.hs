{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Main where
import Control.Monad
import Data.Function
import Data.Monoid
import Data.Either
import Data.List
import Text.Parsec

-- Data definitions
data Identifier = Id String
    deriving (Eq, Show)
data LogicVar = Lit Identifier | Not Identifier
    deriving (Eq, Show)

data Formula = Var LogicVar | And Formula Formula | Or Formula Formula | Negate Formula
    deriving (Eq, Show)

data CNFClause = OrNode [LogicVar]
    deriving (Eq, Show)
data CNF = AndNode [CNFClause]
    deriving (Eq, Show)

-- Pretty-printers for logic formulae

prettyLogicVar (Lit (Id s)) = (s++)
prettyLogicVar (Not (Id s)) = ('!':) . (s++)
prettyCNFClause (OrNode vars) = foldr (.) id . intersperse (" || "++) . map prettyLogicVar $ vars
prettyCNF (AndNode clauses) = foldr (.) id . intersperse (" && "++) . map (showParen True . prettyCNFClause) $ clauses

-- Parser definitions
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

-- Logic-y algorithms
data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving Show

foldMapLeaves f = aux where
    aux (Leaf b) = f b
    aux (Node _ l r) = aux l <> aux r

varName (Lit x) = x
varName (Not x) = x

reduceClauses :: LogicVar -> LogicVar -> CNF -> CNF
reduceClauses x y = removeClausesContaining x . removeFromAllClauses y
    where
        removeClausesContaining v (AndNode clauses) = AndNode (filter (\(OrNode vars) -> not (v `elem` vars)) clauses)
        removeFromAllClauses v (AndNode clauses) = AndNode (map (\(OrNode vars) -> OrNode (filter (/= v) vars)) clauses)

davisPutnamTree :: CNF -> BinTree String (Bool, String)
davisPutnamTree (AndNode clauses) =
    let sortedClauses = AndNode (sortBy (compare `on` (\(OrNode vars) -> length vars)) clauses) in
    -- TODO: {tautological elimination, subsumption elimination, pure literal elimination, unit rule} optimization passes
    case sortedClauses of
        AndNode [] -> Leaf (True, "no clauses left")
        AndNode ((OrNode []):_) -> Leaf (False, "found an empty subclause")
        AndNode ((OrNode (var:_)):_) -> let
            i@(Id name) = varName var
            tag = "branching on " <> name
            leftResult = reduceClauses (Lit i) (Not i) sortedClauses
            rightResult = reduceClauses (Not i) (Lit i) sortedClauses
            in Node tag (davisPutnamTree leftResult) (davisPutnamTree rightResult)

satisfiable :: CNF -> Bool
satisfiable = getAny . foldMapLeaves (Any . fst) . davisPutnamTree

-- Testing
sampleFormulaeStrings = ["(~a || b) && (a)", "(a) && (a)", "(a) && (~a)", "(a || ~a)"]
sampleFormulae = rights . map (parse parseCNF "") $ sampleFormulaeStrings

--main = forever (getLine >>= print . parse parseCNF "")

main = do
    forM_ sampleFormulae $ \phi -> do
        print phi
        putStrLn $ prettyCNF phi ""
        putStrLn $ if satisfiable phi then "Is satisfiable" else "Is not satisfiable"
        putStrLn $ replicate 5 '-'
