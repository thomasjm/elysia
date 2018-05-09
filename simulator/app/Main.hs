{-# LANGUAGE FlexibleContexts, TemplateHaskell, ScopedTypeVariables, QuasiQuotes, OverloadedStrings, NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Char (toLower)
import Data.Convertible
import Data.Maybe
import Data.Random
import Data.Random.Distribution.Dirichlet
import Data.Random.List
import Data.String.Interpolate
import Network.HTTP
import Network.URI.Encode as URI
import System.Random.MWC (create)
import Text.HTML.TagSoup
import Util

-- * Generating random users with metadata and preferences

data Gender = Male | Female deriving Show
data Archetype = Archetype { age :: Int, gender :: Gender, preferences :: Preferences } deriving Show
type Preferences = [Double] -- Animals, vehicles, plants

$(A.deriveJSON (A.defaultOptions { A.constructorTagModifier = fmap toLower }) ''Gender)
$(A.deriveJSON A.defaultOptions ''Archetype)

archetypes = [Archetype 65 Male [0.05, 0.9, 0.05] -- Old man
             , Archetype 16 Female [0.3, 0.0, 0.7] -- Teenage girl
             , Archetype 25 Male [0.5, 0.5, 0.0] -- Twenty-something male
             ]

randomArchetype :: RVar Archetype
randomArchetype = do
  archetype@(Archetype {age, preferences}) <- randomElement archetypes
  ageFuzz :: Double <- normal 0 3
  fuzzedPreferences <- dirichlet preferences
  return $ archetype { age = age + round ageFuzz
                     , preferences = fmap (roundTo 2) fuzzedPreferences }

-- * Hitting the backend and parsing out items

data Item = Item { ident :: String, category :: String } deriving Show

runSingleTest mwc = do
  user <- sampleFrom mwc randomArchetype
  html <- hitBackend user
  putStrLn [i|HTML: #{html}|]
  let rawItems = filter (~== ("<div class=item>" :: String)) $ parseTags html
  let items = [Item (fromMaybe "unknown" (lookup "data-item-id" attrs))
                    (fromMaybe "unknown" (lookup "data-item-category" attrs))
              | TagOpen _ attrs <- rawItems]
  putStrLn [i|Items: #{items}|]

printSampleUsers = create >>= (replicateM 20 . flip sampleFrom randomArchetype) >>= mapM_ print
runTest = create >>= runSingleTest
main = runTest
