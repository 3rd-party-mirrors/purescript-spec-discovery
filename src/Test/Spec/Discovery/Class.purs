module Test.Spec.Discovery.Class
  ( class MappableSpec
  , class SequenceableSpec
  , mapSpec
  , sequenceSpec
  , sequenceSpecM
  , (<.>)
  ) where

import Prelude
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec (SpecT, mapSpecTree)

class
  (Monad m2, MappableSpec m1 m2) <= SequenceableSpec m1 m2 where
  sequenceSpec :: ∀ g i a. SpecT g i m1 a -> SpecT g i m2 a -> SpecT g i m2 a

instance sequenceableSpec :: (Monad m2, MappableSpec m1 m2) => SequenceableSpec m1 m2 where
  sequenceSpec s1 s2 = do
    _ <- mapSpec s1
    s2

sequenceSpecM ::
  ∀ f g i m1 m2 a.
  Monad f =>
  SequenceableSpec m1 m2 =>
  f (SpecT g i m1 a) ->
  f (SpecT g i m2 a) ->
  f (SpecT g i m2 a)
sequenceSpecM fs1 fs2 = do
  s1 <- fs1
  s2 <- fs2
  pure $ sequenceSpec s1 s2

infixl 2 sequenceSpecM as <.>

class MappableSpec m1 m2 where
  mapSpec :: ∀ g i a. SpecT g i m1 a -> SpecT g i m2 a

mapSpec_ :: ∀ g i m1 m2 a. Functor m2 => m1 ~> m2 -> SpecT g i m1 a -> SpecT g i m2 a
mapSpec_ nt = mapSpecTree nt identity

instance identityMappableSpec :: (Applicative m) => MappableSpec Identity m where
  mapSpec = mapSpec_ (pure <<< unwrap)

instance effectMappableSpec :: (MonadEffect m) => MappableSpec Effect m where
  mapSpec = mapSpec_ liftEffect

instance affMappableSpec :: (MonadAff m) => MappableSpec Aff m where
  mapSpec = mapSpec_ liftAff
