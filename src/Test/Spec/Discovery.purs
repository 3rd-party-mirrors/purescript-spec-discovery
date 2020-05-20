module Test.Spec.Discovery (discover) where

import Prelude
import Data.Traversable (sequence_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Test.Spec (SpecT)

foreign import getSpecs ::
  ∀ g i m a.
  EffectFn2 String String (Array (SpecT g i m a))

discover ::
  ∀ m g i m'.
  MonadEffect m =>
  Applicative m' =>
  String ->
  String ->
  m (SpecT g i m' Unit)
discover pattern property = runEffectFn2 getSpecs pattern property >>= (pure <<< sequence_) # liftEffect
