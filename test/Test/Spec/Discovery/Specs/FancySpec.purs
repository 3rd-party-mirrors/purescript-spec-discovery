module Test.Spec.Discovery.Specs.FancySpec (fancySpec) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.ST (ST)
import Control.Monad.State (StateT, execStateT, get, lift, put)
import Data.Array (length, range)
import Data.Array.ST (STArray, run, unsafeThaw)
import Data.Array.ST.Partial (peek, poke)
import Data.Either (fromRight)
import Data.String (trim)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, afterAll_, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (fail, shouldEqual)

fancySpec :: SpecT Aff Unit Aff Unit
fancySpec = do
  beforeAll_ (log $ stripMargin startMsg)
    $ afterAll_ (log $ stripMargin endMsg) do
        greetSpec
        stripMarginSpec
        quicksortSpec

startMsg :: String
startMsg =
  "     | Fancy spec will test three specs in parallel.                  "
    <> "|                                                                "
    <> "| The first one will test the super ultra difficult to understand"
    <> "| function greet.                                                "
    <> "|                                                                "
    <> "| The second will test the stripMargin function, witch allows us "
    <> "| to write a message formatted like this string and just removes "
    <> "| the pattern.                                                   "
    <> "|                                                                "
    <> "| The third will test two quicksort implementations, one in pure-"
    <> "| script, the other in javascript.                               "

endMsg :: String
endMsg =
  "     | And thats it for these 'extensive' and exhaustive tests... "
    <> "| Have a good day!                                           "
    <> "| (•_•) ( •_•)>⌐■-■ (⌐■_■)                                   "

--------------------------------------------------------------------------------
-- Really complex functions to be tested...(̿▀̿‿ ̿▀̿ ̿)
--------------------------------------------------------------------------------
--
-- greet
greet :: String -> String
greet name = "Hello " <> name <> "!!!"

greetSpec :: ∀ m. MonadEffect m => SpecT Aff Unit m Unit
greetSpec = do
  describe "greet" do
    it "should say hello to Jane" do
      let
        jane = "Jane"
      greet jane `shouldEqual` "Hello Jane!!!"

--
-- stripMargin
stripMargin :: String -> String
stripMargin =
  let
    mkPattern = unsafePartial fromRight <<< (flip regex global)

    replacePattern = Regex.replace (mkPattern "[\\ \t]*\\|[\\ \t]*") "\n"
  in
    trim >>> replacePattern

stripMarginSpec :: ∀ m. MonadEffect m => SpecT Aff Unit m Unit
stripMarginSpec = do
  describe "stripMargin" do
    it "should replace pattern \"|\" by \"\\n\" preserving string format" do
      let
        expectedStartMsg =
          "\nFancy spec will test three specs in parallel."
            <> "\n"
            <> "\nThe first one will test the super ultra difficult to understand"
            <> "\nfunction greet."
            <> "\n"
            <> "\nThe second will test the stripMargin function, witch allows us"
            <> "\nto write a message formatted like this string and just removes"
            <> "\nthe pattern."
            <> "\n"
            <> "\nThe third will test two quicksort implementations, one in pure-"
            <> "\nscript, the other in javascript."

        expectedEndMsg =
          "\nAnd thats it for these 'extensive' and exhaustive tests..."
            <> "\nHave a good day!"
            <> "\n(•_•) ( •_•)>⌐■-■ (⌐■_■)"
      stripMargin startMsg `shouldEqual` expectedStartMsg
      stripMargin endMsg `shouldEqual` expectedEndMsg

--
-- quicksort
foreign import jsQuicksort :: ∀ a. Array a -> Effect (Array a)

quicksortSpec :: ∀ m. MonadEffect m => SpecT Aff Unit m Unit
quicksortSpec = do
  describe "reference equality" do
    it "arrays with same values are not the same at memory" do
      let
        array1 = [ 98, 84, 557, 4898, 2 ]

        array2 = [ 98, 84, 557, 4898, 2 ]
      --
      array1 `shouldNotRefEqual` array2
      array1 `shouldEqual` [ 98, 84, 557, 4898, 2 ]
      array2 `shouldEqual` [ 98, 84, 557, 4898, 2 ]
  --
  describe "quicksort"
    $ parallel do
        describe "javascript" do
          it "sort an array in place" do
            let
              array1 = [ 98, 84, 557, 4898, 2 ]
            array2 <- liftEffect $ jsQuicksort array1
            --
            array1 `shouldRefEqual` array2
            array1 `shouldEqual` [ 2, 84, 98, 557, 4898 ]
            array2 `shouldEqual` [ 2, 84, 98, 557, 4898 ]
        --
        describe "purescript" do
          it "sort an array in place" do
            let
              array1 = [ 864, 54894, 548 ]
            array2 <- liftEffect $ psQuicksort array1
            --
            array1 `shouldRefEqual` array2
            array1 `shouldEqual` [ 548, 864, 54894 ]
            array2 `shouldEqual` [ 548, 864, 54894 ]

-- from https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell
psQuicksort :: ∀ a. Ord a => Array a -> Effect (Array a)
psQuicksort array = do
  let
    mArray :: ∀ h. ST h (STArray h a)
    mArray = do
      stArray <- unsafeThaw array
      quicksort stArray 0 (length array)
      pure stArray
  pure $ run mArray
  where
  quicksort :: ∀ h. STArray h a -> Int -> Int -> ST h Unit
  quicksort arr start end =
    when (start + 1 < end)
      $ do
          pivotIndex <- partition arr start end
          quicksort arr start pivotIndex
          quicksort arr (pivotIndex + 1) end

  partition :: ∀ h. STArray h a -> Int -> Int -> ST h Int
  partition arr start end = do
    pivotElement <- unsafePartial $ peek start arr
    finalPivotIndex <-
      execStateT
        (traverse (partitionLoop arr pivotElement) $ range (start + 1) (end - 1))
        (start + 1)
    swap arr start (finalPivotIndex - 1)
    pure $ finalPivotIndex - 1

  partitionLoop :: ∀ h. STArray h a -> a -> Int -> StateT Int (ST h) Unit
  partitionLoop arr pivotElement i = do
    pivotIndex <- get
    thisElement <- lift $ unsafePartial $ peek i arr
    when (thisElement <= pivotElement)
      $ do
          lift $ swap arr i pivotIndex
          put (pivotIndex + 1)

  swap :: ∀ h. STArray h a -> Int -> Int -> ST h Unit
  swap arr leftIndex rightIndex = do
    elem1 <- unsafePartial $ peek leftIndex arr
    elem2 <- unsafePartial $ peek rightIndex arr
    unsafePartial $ poke leftIndex elem2 arr
    unsafePartial $ poke rightIndex elem1 arr

-- private impl
foreign import reallyUnsafeRefEq :: ∀ a b. a -> b -> Boolean

infix 4 reallyUnsafeRefEq as ===

shouldRefEqual ::
  ∀ m t1 t2.
  MonadThrow Error m =>
  Show t1 =>
  Show t2 =>
  t1 ->
  t2 ->
  m Unit
shouldRefEqual v1 v2 =
  when (not $ v1 === v2)
    $ (fail $ "v1 shouldRefEqual v2, but (v1 = " <> show v1 <> ") =≠= (v2 = " <> show v2 <> ")")

shouldNotRefEqual ::
  ∀ m t1 t2.
  MonadThrow Error m =>
  Show t1 =>
  Show t2 =>
  t1 ->
  t2 ->
  m Unit
shouldNotRefEqual v1 v2 =
  when (v1 === v2)
    $ (fail $ "v1 shouldNotRefEqual v2, but (v1 = " <> show v1 <> ") === (v2 = " <> show v2 <> ")")
