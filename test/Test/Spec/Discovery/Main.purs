module Test.Spec.Discovery.Main (main) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (Spec, SpecT)
import Test.Spec.Discovery (discover)
import Test.Spec.Discovery.Class ((<.>))
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

-- Discovery specs from old example
-- Discovered spec type is concrete and known in advance:`spec :: Spec Unit`
-- specs :: ∀ m g i m'. MonadEffect m => Applicative m' => m (SpecT g i m' Unit)
-- specs :: ∀ m. MonadEffect m => m (Spec Unit)
specs :: Aff (Spec Unit)
specs = discover "Test.Spec.Discovery.Specs.Discovery.*Spec" "spec"

-- Discovered spec type is concrete and known in advance: `fancySpec :: SpecT Aff Unit Aff Unit`
-- fancySpecs :: ∀ m. MonadEffect m => m (SpecT Aff Unit Aff Unit)
fancySpecs :: Aff (SpecT Aff Unit Aff Unit)
fancySpecs = discover "Test.Spec.Discovery" "fancySpec"

main :: Effect Unit
main = launchAff_ $ runT =<< specs <.> fancySpecs

runT :: SpecT Aff Unit Aff Unit -> Aff Unit
runT = runSpecT defaultConfig [ consoleReporter ] >>> (_ >>= void)
