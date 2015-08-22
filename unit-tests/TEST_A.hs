{-# LANGUAGE DeriveDataTypeable #-}
module TEST_A where
import Data.Typeable

data Dat = TEST_A deriving (Typeable, Read, Show, Eq, Ord)
