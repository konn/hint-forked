{-# LANGUAGE DeriveDataTypeable #-}
module TEST_B (Dat(..)) where
import Data.Typeable

data Dat = TEST_B deriving (Typeable, Read, Show, Eq, Ord)
