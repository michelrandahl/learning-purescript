module NodePath where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Aff (Aff)

type FilePath = String

foreign import joinPath :: FilePath -> FilePath -> FilePath

foreign import joinPathImpl :: Fn2 FilePath FilePath FilePath
joinPath2 :: FilePath -> FilePath -> FilePath
joinPath2 p1 p2 = runFn2 joinPathImpl p1 p2

foreign import resolveImpl :: Fn2 (Array FilePath) FilePath (Effect FilePath)
resolve :: Array FilePath -> FilePath -> Effect FilePath
resolve paths path = runFn2 resolveImpl paths path

foreign import readTextFileImpl :: FilePath -> EffectFnAff String
readFile :: FilePath -> Aff String
readFile path = fromEffectFnAff $ readTextFileImpl path

