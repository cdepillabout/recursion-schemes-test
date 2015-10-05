{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

-- Playing around with recursion schemes from the two blog posts here:
-- http://jozefg.bitbucket.org/posts/2014-05-19-like-recursion-but-cooler.html
-- http://jozefg.bitbucket.org/posts/2014-06-14-like-recursion-but-cooler-2.html
--
-- And the paper here:
-- http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf

import Data.Functor.Foldable
import Prelude hiding (Foldable)

data MyList a = MyCons a (MyList a) | MyNil
    deriving Show

data BList a b = BCons a b | BNil
    deriving (Functor, Show)

type instance Base (MyList a) = BList a

instance Foldable (MyList a) where

    -- project :: t -> (Base t) t
    -- project :: MyList a -> (Base (MyList a)) (MyList a)
    project :: MyList a -> BList a (MyList a)
    project (MyCons a b) = BCons a b
    project MyNil        = BNil

    -- cata :: (Base t b -> b) -> t -> b
    -- cata :: (Base (MyList a) b -> b) -> MyList a -> b
    -- cata :: (BList a b -> b) -> MyList a -> b

    -- para :: (Base t (t, b) -> b) -> t -> b
    -- para :: (Base (MyList a) (MyList a, b) -> b) -> t -> b
    -- para :: (BList a (MyList a, b) -> b) -> t -> b

-- In the webpage, these two are only defined as partial functions, with
-- only the BCons cases defined...
out :: Fix (BList a) -> MyList a
out (Fix (BCons a rest)) = MyCons a (out rest)
out (Fix BNil)           = MyNil
into :: MyList a -> Fix (BList a)
into (MyCons a rest) = Fix (BCons a $ into rest)
into MyNil           = Fix BNil

cataTest :: BList Int Int -> Int
cataTest (BCons i rest) = i + rest
cataTest BNil = 0

paraTest :: BList Int (MyList Int, String) -> String
paraTest (BCons i (myList, rest)) = show i ++ rest ++ "\n\t" ++ show myList ++ "\n"
paraTest BNil = "BNil"

myMap :: forall a b . (a -> b) -> MyList a -> MyList b
myMap f = cata mapper
  where
    mapper :: BList a (MyList b) -> MyList b
    mapper BNil = MyNil
    mapper (BCons a rest) = f a `MyCons` rest


mySum :: Num a => MyList a -> a
mySum (MyCons a rest) = a + mySum rest
mySum MyNil = 0

sumTails :: MyList Int -> MyList Int
sumTails = para summer
  where
    summer :: BList Int (MyList Int, MyList Int) -> MyList Int
    summer (BCons a (list, rest)) = MyCons (a + mySum list) rest
    summer BNil                   = MyNil


data Op = Plus | Sub | Mult | Div
    deriving Show

data Foo = Num Int           -- Numeric literals
         | String String     -- String literals
         | Binop Op Foo Foo  -- Primitive operation
         | Fun String Foo    -- Lambda/Abstraction over terms
         | App Foo Foo       -- Application
         | Var String        -- Variables
    deriving Show

compute :: Op -> Int -> Int -> Int
compute Plus = (+)
compute Sub  = (-)
compute Mult = (*)
compute Div  = div


data FooB a = NumB Int
            | StringB String
            | BinopB Op a a
            | FunB String a
            | AppB a a
            | VarB String
    deriving (Functor, Show)

type instance Base Foo = FooB

instance Foldable Foo where
    -- project :: t -> (Base t) t
    -- project :: Foo -> (Base Foo) Foo
    project :: Foo -> FooB Foo
    project (Num a)        = NumB a
    project (String a)     = StringB a
    project (Binop op a b) = BinopB op a b
    project (Fun v a)      = FunB v a
    project (App a b)      = AppB a b
    project (Var a)        = VarB a

-- reverse of project
-- rProject :: Base Foo Foo -> Foo
rProject :: FooB Foo -> Foo
rProject (NumB a)        = Num a
rProject (StringB a)     = String a
rProject (BinopB op a b) = Binop op a b
rProject (FunB v a)      = Fun v a
rProject (AppB a b)      = App a b
rProject (VarB a)        = Var a

-- This is reduce without using recursion-schemes.
reduceBad :: Foo -> Foo
reduceBad (Binop op (Num a) (Num b)) = Num $ compute op a b -- The reduction
reduceBad a                          = a

-- This is reduce with recursion-schemes.
-- reduce :: Base Foo Foo -> Foo
reduce :: FooB Foo -> Foo
reduce (BinopB op (Num a) (Num b)) = Num $ compute op a b -- The reduction
reduce a                           = rProject a

constFold :: Foo -> Foo
constFold = cata reduce


test :: Foo
test = Binop Plus (Num 1) (Binop Mult (Num 2) (Num 3))

optimized :: Foo
optimized = constFold test


someFunc :: IO ()
someFunc = putStrLn "someFunc"

