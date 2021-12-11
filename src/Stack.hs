{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Stack where
import Prelude

data StackState = EmptyStack | NotEmptyStack
newtype Stack a = Stack [a] deriving (Show, Foldable)

empty :: Stack a
empty = Stack []
isEmpty  :: Stack a -> StackState
isEmpty s = if  null s then  EmptyStack else NotEmptyStack

push :: a -> Stack a -> Stack a
push x (Stack xs)= Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

drain :: Stack a -> [a]
drain (Stack []) = []
drain (Stack (x:xs)) = x: drain (Stack xs) 