{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Main where
import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Function
import Data.List
import Data.Monoid
import Text.Parsec hiding (State)
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D

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

davisPutnamTree :: CNF -> BinTree (CNF, String) (Bool, String)
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
            in Node (sortedClauses, tag) (davisPutnamTree leftResult) (davisPutnamTree rightResult)

satisfiable :: CNF -> Bool
satisfiable = getAny . foldMapLeaves (Any . fst) . davisPutnamTree

-- Diagram generation
(#) = (D.#)

treeDiagram :: BinTree (CNF, String) (Bool, String) -> D.Diagram D.B
treeDiagram tree = evalState (aux tree) (0 :: Integer) where
    aux (Leaf (sat, comment)) = return $ D.circle 1 # D.fc (if sat then D.green else D.red) # D.lw 0.5
    aux (Node (cnf, comment) l r) = let
        text s = D.text s <> (D.rect (genericLength s) 1 # D.lw D.none)
        node = (D.vsep 1 [text (prettyCNF cnf ""), text comment])
        freshName = modify (+1) >> get
        (vl, vr) = (D.r2 (-1.0, -0.5), D.r2 (1.0,-0.5))
        arrowOptions = D.def D.& D.headLength D..~ D.local 0.5 D.& D.shaftStyle D.%~ D.lw 1
        in do
            [lname, rname, nname] <- replicateM 3 freshName
            l' <- D.named lname <$> aux l
            r' <- D.named rname <$> aux r
            let node' = D.named nname node
            let connect = D.connect' arrowOptions nname
            return $ (node' `D.appends` [(vl, l'), (vr, r')]) # connect lname # connect rname

-- Testing
sampleFormulaeStrings = ["(~a || b) && (a)", "(a) && (a)", "(a) && (~a)", "(a || ~a)", "(a || ~b || c) && (~d || e || ~f || g)"]
sampleFormulae = rights . map (parse parseCNF "") $ sampleFormulaeStrings

--main = forever (getLine >>= print . parse parseCNF "")

main = do
    forM_ (zip [0..] sampleFormulae) $ \(i, phi) -> do
        print phi
        let s = 1000 in D.renderSVG ("formula" <> show i <> ".svg") (D.mkSizeSpec (D.r2 (Just s, Just s))) (treeDiagram (davisPutnamTree phi))
        putStrLn $ prettyCNF phi ""
        putStrLn $ if satisfiable phi then "Is satisfiable" else "Is not satisfiable"
        putStrLn $ replicate 5 '-'
