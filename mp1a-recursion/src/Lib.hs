--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs)
    | n > 0 = x : mytake (n-1) xs
    | otherwise = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs)
    | n > 0 = mydrop (n-1) xs
    | otherwise = x:xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xx = aux xx []
    where
        aux [] acc = acc
        aux (x:xs) acc = aux xs (x:acc)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] yy = yy
app (x:xs) yy = x : app xs yy

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist xx = aux xx 0
    where
        aux [] acc = acc
        aux (x:xs) acc = aux xs (acc + x)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xx yy =
    let xxyy = myzip xx yy
    in aux xxyy
    where
        aux [] = []
        aux ((x,y):xys) = (x+y) : aux xys

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined
