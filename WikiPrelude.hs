-- `WikiPrelude` - A custom Prelude for Wiki parsing
-- =================================================

-- The Haskell standard library is called the 'Prelude'. There is general
-- agreement that the Prelude is not very good code and should be replaced with
-- something else.  There is absolutely no agreement on what it should be replaced
-- with.

-- I built this code on an alternative Haskell standard library, the Classy
-- Prelude, because it's got a good idea of how to deal with different data types
-- such as Texts and ByteStrings. Instead of prefixed functions that deal with one
-- data type, ClassyPrelude uses generic functions that can apply to any data type
-- that does the right things, because it defines these functions in terms of
-- type classes.

-- Instead of having to use `T.append` to append texts, and `Char8.append` to
-- append ByteStrings, or whatever, we just use `append` to append things that can
-- be appended. This leads to cleaner code and easier refactoring.

-- It comes at a price: when a type signature in the code is slightly wrong,
-- Haskell with the Classy Prelude will output error messages that are more
-- confusing than usual, because everything is a level of abstraction removed from
-- the actual types you mean to use.

-- Speaking of abstraction, let's talk about two important concepts that happen
-- to be typeclasses with frightening names: Monoids and Monads.


-- Monoids and monads, oversimplified
-- ----------------------------------

-- A [classic joke][] about Haskell defines these terms: "A monad is a monoid in
-- the category of endofunctors, what's the problem?"

-- [classic joke]: http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html

-- It's funny because it's true. Haskell works best when you embrace its
-- poorly-named mathematical abstractions, and monads and monoids are the ones
-- that are going to come up in this code. But in actual Haskell code, the full
-- generality of the mathematical abstraction usually doesn't matter; it's all
-- about how you use it.

-- So let's oversimplify what these things are, the way we oversimplify other
-- mathematical concepts like "matrix" when programming.

-- If you're a Haskell programmer reading this, you'll find this section to be old
-- hat. I don't know many Haskell programmers, though. This documentation is for
-- other people who come across this code and are just familiar with functional
-- programming in general.

-- Monoids are things you can join together
-- ----------------------------------------

-- A **monoid** is a type of thing that can be joined in a way that's like
-- concatenation. The type of thing also needs to have an empty value, `ø`, which
-- when you join it with something just gives you back that thing.

-- Some monoids you'll encounter in this code are Unicode text, ByteStrings, and
-- lists. Sets also work, joined using the "union" operation.

-- (A mathematician might say I'm overlooking some monoids that are a big deal,
-- like addition of numbers. But you wouldn't realistically *use* a monoid to add
-- numbers. You'd use `+` to add numbers.)

-- When I'm willing to call all these sequencey things Monoids, then instead of
-- having to use awkwardly-namespaced functions for dealing with all these types
-- separately (like `T.append` for text, versus `BS.append` for bytestrings), I
-- can use `mappend` to append whatever monoidy things I have, and `mempty` or
-- `ø` to get an empty one.

-- By the way, Haskell programmers show their apprecation for functions they find
-- really important by giving them infix operators. So `mappend list1 list2` is
-- also spelled `list1 <> list2`, or in fancy Unicode, `list1 ⊕ list2`.

-- In short, "Monoid" should have been named "Joinable". But I can't change
-- that name very easily.

-- Monads are stateful things you can do
-- -------------------------------------

-- A **monad** is a way to do stateful things in sequence. The advantage of using
-- a monad is that it keeps track of the state for you while you just return a
-- result. Without monads, you might have to write functions that take in
-- `(actualInput, state)` and return `(actualOutput, newState)`, which would be
-- repetitive and error-prone.

-- In Haskell, doing any sort of I/O requires an IO monad: your code is changing
-- the state of what it's read from and written to the rest of the system.

-- Parsing is a monad. Your state is where you are in the input. When you parse
-- something and move the cursor forward through the input, that modifies the
-- state.

-- Monads are important enough to Haskell that they get their own syntax, the `do`
-- block, which just lets you list a bunch of state-changing things you need to
-- do to a monad, in order.

-- Because IO and parsing are the same kind of thing, they look similar in the
-- type system. A function of type `IO Text` is a function that does some IO and
-- then returns some Text. A function of type `Parser Text` is a function that
-- parses some input and then returns some Text.

-- In short, "Monad" should have been named "Sequenceable".

-- Here's where the actual code starts
-- -----------------------------------

-- Every module starts with a LANGUAGE line telling the Haskell compiler
-- which optional language features to use.

-- The ones we'll see a lot are
-- `NoImplicitPrelude`, telling it not to import the Prelude because we're
-- defining our own right here, and `OverloadedStrings`, which lets us
-- use quoted literals as whatever type they need to be, instead of them
-- ending up as a String, an awful type you never want to use because it's
-- a linked list of characters.

-- We also turn on `UnicodeSyntax` a lot so we can bling out our code.

-- Various LANGUAGE lines throughout this codebase will turn on other features
-- such as `FlexibleContexts`, and honestly I have no clear idea what they do,
-- except that the compiler tells me things like "I can't compile this unless you
-- turn on `FlexibleContexts`".

-- `OverloadedStrings` would be dangerous to have on while we're still defining
-- what strings even are, so it's not turned on in the WikiPrelude.

{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, FlexibleContexts #-}

-- The WikiPrelude is a small extension of the ClassyPrelude, designed to
-- include some more types and functions that we'll need throughout the parser.

-- Here's what we're exporting from the module:

module WikiPrelude (
  module ClassyPrelude,
  module Data.String.Conversions,
  module Data.LanguageType,
  module Control.Monad.Writer,
  replace, splitOn, stripSpaces, dropAround, dropWhileEnd, toTitle,
  breakOn, breakOnEnd, listTakeWhile, listDropWhile, hGetContentsLazy,
  get, getAll, getPrioritized, nonEmpty, hasKey,
  println, ø, (∈), (⊕),
  (??),
  (.||),(.&&),
  ) where

-- Some of these exports are just re-exporting things that we import en masse:

import ClassyPrelude hiding (takeWhile, splitFirst)
import qualified ClassyPrelude as P
import Data.String.Conversions hiding ((<>), (⊕))
import qualified Data.ByteString.Lazy as BSL
import Data.LanguageType
import Data.Monoid.Unicode ((⊕), (∅))
import Control.Monad.Writer (Writer, writer, pass, runWriter, execWriter)
import qualified Data.Text as T

-- Text operations
-- ---------------

-- These are functions that apply to Text that for some reason didn't make it into
-- the ClassyPrelude.

replace = T.replace
splitOn = T.splitOn
breakOn = T.breakOn
breakOnEnd = T.breakOnEnd
dropAround = T.dropAround
dropWhileEnd = T.dropWhileEnd
toTitle = T.toTitle
hGetContentsLazy = BSL.hGetContents

-- Another kind of standard thing we need to do is trim spaces from the start and
-- end of a string:

stripSpaces :: Text -> Text
stripSpaces = dropAround (\c -> c == ' ' || c == '\n')

-- Writing any sort of text to stdout:

println :: Text -> IO ()
println = putStrLn


-- List operations
-- ---------------

-- The name `takeWhile` has conflicting definitions in ClassyPrelude and
-- Attoparsec, so we need to rename the ClassyPrelude one.

listTakeWhile :: (a -> Bool) -> [a] -> [a]
listTakeWhile = P.takeWhile

listDropWhile :: (a -> Bool) -> [a] -> [a]
listDropWhile = P.dropWhile


-- Unicode shenanigans
-- -------------------

-- We can use Unicode operators to work with monoids, and we might as well do so
-- because we'll be working with a lot of monoids.

-- One thing I want to do is define the empty monoid (the value that, when you
-- join it with value X, you get back X) as the letter ø. I know it's supposed to
-- be the math symbol ∅. You have to put that one in parentheses because it's a
-- symbol and Haskell's parser thinks it's supposed to be an infix operator. But ø
-- is a name, and that's exactly what we need.

ø :: Monoid α => α
ø = (∅)

-- `∈` is the element-of operator, and actually having it as an infix operator is
-- rather nice. We have to make sure it refers to the ClassyPrelude version of
-- `elem`, complete with its type signature (which I just copied and pasted).

(∈) :: (MonoFoldable c, Eq (Element c)) => Element c -> c -> Bool
(∈) = elem

-- Mapping operations
-- ------------------

-- In many situations we have a mapping whose values are sequences. This lets us
-- write the convenient `get` function, which looks up a key in the mapping, or
-- returns an empty sequence if it's not there. This is often easier than checking
-- cases of `Maybe` values.

-- What I'm calling a sequence is what Haskell calls a monoid -- see the section
-- "Monoids are things you can concatenate" above.


get :: (IsMap map, Monoid (MapValue map)) => ContainerKey map -> map -> MapValue map
get = findWithDefault ø

-- `getPrioritized` is like `get`, but tries looking up multiple different keys
-- in priority order. It returns the empty value only if it finds none of them.

getPrioritized :: (IsMap map, Monoid (MapValue map)) => [ContainerKey map] -> map -> MapValue map
getPrioritized (key:rest) map = findWithDefault (getPrioritized rest map) key map
getPrioritized [] map         = ø

-- `getAll` is a step in `getPrioritized`. It takes a list of keys and a mapping,
-- and returns the list of values of those keys that exist.

getAll :: (IsMap m, Monoid (MapValue m)) => [ContainerKey m] -> m -> [MapValue m]
getAll keys m = catMaybes (map (\key -> lookup key m) keys)

-- Sometimes we don't want a Maybe, we want to do something different based on
-- whether a key is present or not:

hasKey :: IsMap map => ContainerKey map -> map -> Bool
hasKey key map = isJust (lookup key map)

-- Perhaps we've gotten some values that may or may not be empty using `get`, and
-- now we want to turn them back into a proper `Maybe`. This function replaces
-- `Maybe ø` with `Nothing`:

nonEmpty :: (Monoid α, Eq α) => Maybe α -> Maybe α
nonEmpty val =
  case val of
    Just something -> if (something == ø) then Nothing else val
    Nothing -> Nothing


-- use `??` as infix operator for defaulting a Maybe value
(??) = flip fromMaybe 

-- use `.||` and `.&&` to combine functions that produce a Bool
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) x y a = x a || y a

-- use `|||` and `&&&` to combine functions that produce a Bool
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) x y a = x a && y a

