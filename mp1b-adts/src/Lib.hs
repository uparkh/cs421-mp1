--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x l) = x : cons2list l

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' = undefined

--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp = undefined
