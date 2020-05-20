module Test.Spec.Discovery.Main (main) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (Spec)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- Discovery specs from old example
-- Discovered spec type is concrete and known in advance:`spec :: Spec Unit`
-- specs :: ∀ m g i m'. MonadEffect m => Applicative m' => m (SpecT g i m' Unit)
-- specs :: ∀ m. MonadEffect m => m (Spec Unit)
specs :: Aff (Spec Unit)
specs = discover "Test.Spec.Discovery.Specs.Discovery.*Spec" "spec"

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] =<< specs
