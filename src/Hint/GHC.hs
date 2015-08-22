module Hint.GHC (
    module GHC,
    module Outputable,
    module ErrUtils, Message,
    module Pretty,
    module DriverPhases,
    module StringBuffer,
    module Lexer,
    module Parser,
    module DynFlags,
    module FastString,
#if __GLASGOW_HASKELL__ >= 610
    module Control.Monad.Ghc,
    module HscTypes,
    module Bag,
#endif
#if __GLASGOW_HASKELL__ >= 608
    module PprTyThing,
#elif __GLASGOW_HASKELL__ < 608
    module SrcLoc,
#endif
#if __GLASGOW_HASKELL__ >= 702
    module SrcLoc,
#endif
#if __GLASGOW_HASKELL__ >= 708
    module ConLike,
#endif
)

where
#if __GLASGOW_HASKELL__ >= 610
import GHC hiding ( Phase, GhcT, runGhcT )
import Control.Monad.Ghc ( GhcT, runGhcT )

import HscTypes ( SourceError, srcErrorMessages, GhcApiError )
import Bag ( bagToList )
#else
import GHC hiding ( Phase )
#endif

import Outputable   ( PprStyle, SDoc, Outputable(ppr),
                      showSDoc, showSDocForUser, showSDocUnqual,
                      withPprStyle, defaultErrStyle )

import ErrUtils     ( mkLocMessage, pprErrMsgBagWithLoc)
#if __GLASGOW_HASKELL__ < 706
import ErrUtils     ( Message )
#else
import ErrUtils     ( MsgDoc ) -- we alias it as Message below
#endif

import Pretty       ( Doc )
import DriverPhases ( Phase(Cpp), HscSource(HsSrcFile) )
import StringBuffer ( stringToStringBuffer )
import Lexer        ( P(..), ParseResult(..), mkPState )
import Parser       ( parseStmt, parseType )
import FastString   ( fsLit )

#if   __GLASGOW_HASKELL__ >= 710
import DynFlags     ( supportedLanguagesAndExtensions, xFlags, xopt, FlagSpec(..) )
#elif __GLASGOW_HASKELL__ >= 700
import DynFlags     ( supportedLanguagesAndExtensions, xFlags, xopt )
#else
import DynFlags     ( supportedLanguages )
#endif

#if __GLASGOW_HASKELL__ >=704
import DynFlags     ( LogAction )
#endif

#if __GLASGOW_HASKELL__ >= 608
import PprTyThing   ( pprTypeForUser )
#elif __GLASGOW_HASKELL__ < 608
import SrcLoc       ( SrcSpan )
#endif

#if __GLASGOW_HASKELL__ >= 702
import SrcLoc       ( mkRealSrcLoc )
#endif

#if __GLASGOW_HASKELL__ >= 708
import ConLike      ( ConLike(RealDataCon) )
#endif

#if __GLASGOW_HASKELL__ >= 706
type Message = MsgDoc
#endif
