import Language.Haskell.Extension
import Text.PrettyPrint

main = putStrLn $ render moduleDoc

moduleDoc :: Doc
moduleDoc =
  vcat [
    text "-- this module was automatically generated. do not edit!",
    text "-- edit util/mk_extensions_mod.hs instead",
    text "module Hint.Extension (Extension(..),",
    text "                       knownExtensions, availableExtensions, asExtension)",
    text "",
    text "where",
    text "",
    text "import Hint.Compat as Compat",
    text "",
    text "-- | List of the extensions known by the interpreter.",
    text "availableExtensions :: [Extension]",
    text "availableExtensions = map asExtension Compat.supportedExtensions",
    text "",
    text "asExtension :: String -> Extension",
    text "asExtension s = if isKnown s",
    text "                  then read s",
    text "                  else let no_s = \"No\" ++ s",
    text "                  in if isKnown no_s then read no_s",
    text "                                     else UnknownExtension s",
    text "  where isKnown e = e `elem` map show knownExtensions",
    text "",
    text "-- | This represents language extensions beyond Haskell 98",
    text "--   that are supported by GHC (it was taken from",
    text "--   Cabal's @Language.Haskell.Extension@)",
    align "data Extension " $
    punctuateL (text "| ") . onFirst (text "= ") $ known ++ [unknown],
    nest 8 $ text "deriving (Eq, Show, Read)",
    text "",
    text "knownExtensions :: [Extension]",
    align "knownExtensions = [" (punctuate comma known ++ [text "]"])
  ]

known :: [Doc]
known = map (text . show) knownExtensions

unknown :: Doc
unknown = text "UnknownExtension String"

align :: String -> [Doc] -> Doc
align s []     = text s
align s (d:ds) = hang (text s <> d) (length s) (vcat ds)

-- punctuateL p [d1, ..., dn] = [d1, p <> d2, ..., p <> dn]
punctuateL :: Doc -> [Doc] -> [Doc]
punctuateL _ []     = []
punctuateL _ [d]    = [d]
punctuateL p (d:ds) = d : map (p <>) ds

onFirst :: Doc -> [Doc] -> [Doc]
onFirst _ [] = []
onFirst p (d:ds) = p <> d : ds
