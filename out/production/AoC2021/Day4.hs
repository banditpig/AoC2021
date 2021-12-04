{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveTraversable #-}
module Day4 where
import Prelude
import Data.Foldable (fold)
      
data Five a = Five a a a a a  deriving ( Show)
instance (Monoid a, Num a ) =>  Semigroup (Five a ) where 
  Five  s t u v w <> Five  s' t' u' v' w' = Five (s <> s')  (t <> t') (u <> u') (v <> v') (w <> w')
  
instance (Monoid a, Num a ) => Monoid (Five a) where
  mempty = Five 0 0 0 0 0
  mappend = (<>)
   
instance Functor Five where
  fmap f (Five s t u v w) = Five  (f s) (f t) (f u) (f v) (f w)
  
instance Applicative Five where 
  pure x = Five x x x x x
  Five fs ft fu fv fw <*> Five s t u v w = Five (fs s) (ft t) (fu u) (fv v) (fw w)
  
instance Foldable Five where
   foldMap f tfive = fold (fmap f tfive) 
     
instance Traversable Five where
   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse  f (Five s t u v w) = Five <$> f s <*> f t <*> f u <*> f v <*> f w
  
  -- Node <$> traverse f l <*> f x <*> traverse f r
newtype Board a = Board (Five (Five a)) deriving (Show,Functor,Foldable, Traversable)





