{-# LANGUAGE KindSignatures #-}

module CommandLine (CommandLineProcessor(..)) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)

data CommandLineProcessor (f :: Type -> Type) = 
  CommandLineProcessor {
      runVersion       ::                    f ()
    , runHelp          ::                    f ()  
    , runInfo          ::                    f ()
    , runWithLocalTime :: NonEmpty String -> f ()
    , runWithUtcTime   :: NonEmpty String -> f ()
  }