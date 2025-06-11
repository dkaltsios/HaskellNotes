data Gold = Gold
            { unGold :: Int
            } deriving (Show , Eq)

data Reward = Reward
              { rewardGold :: Gold
              , rewardSpecial :: Bool
              } deriving (Show , Eq)

instance Semigroup Gold where
    Gold x <> Gold y = Gold (x+y)

instance Monoid Gold where
    mempty = Gold 0

instance Semigroup Reward where
    Reward x s1 <> Reward y s2 = if Reward (x<>y) (s1||s2)

instance Monoid Reward where 
    mempty = Reward mempty False

data Tree a = Node a [ Tree a ] deriving (Show , Eq)

instance Functor Tree where
    fmap f (Node x subtrees) = Node (f x) (map (fmap f) subtrees)
    

data Pair a = Pair a a deriving (Show , Eq)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- Propositional Logic DSL

-- Define the type for logical propositions
data Prop
  = Var String
  | And Prop Prop
  | Or Prop Prop
  | Implies Prop Prop
  | Not Prop
  deriving (Show, Eq)

-- Meta-level logical equivalence (IFF)
iff :: Prop -> Prop -> Prop
iff a b = And (Implies a b) (Implies b a)

-- Environment: mapping from variable names to Boolean values
type Env = [(String, Bool)]

-- Evaluation function: computes the value of a proposition given an environment
eval :: Prop -> Env -> Maybe Bool
eval (Var x) env = lookup x env

eval (And p1 p2) env = do
  v1 <- eval p1 env
  v2 <- eval p2 env
  return (v1 && v2)

eval (Or p1 p2) env = do
  v1 <- eval p1 env
  v2 <- eval p2 env
  return (v1 || v2)

eval (Implies p1 p2) env = do
  v1 <- eval p1 env
  v2 <- eval p2 env
  return (not v1 || v2)

eval (Not p) env = do
  v <- eval p env
  return (not v)

-- Generate all possible environments (truth table rows) for given variable names
truthtable :: [String] -> [Env]
truthtable [] = [[]]
truthtable (v:vs) =
  [ (v, val) : env | val <- [False, True], env <- truthtable vs ]

-- Compute all possible evaluations of a proposition across a truth table
compute :: Prop -> [String] -> [Maybe Bool]
compute prop vars = map (eval prop) (truthtable vars)

