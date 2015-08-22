0.4.2.3
-------

 * It builds against 7.4.2 (not 7.4.1), so update the constraints.


0.4.2.2
-------

 * Builds with ghc 7.10
 * Builds again with ghc 7.4
 * Drops dependency on utf8-string


0.4.2.1
-------

 * Better error reporting (thanks to Corentin Dupont)

0.4.2.0
-------

 * Based on exceptions-0.6

0.4.1.0
-------

 * Based on exceptions-0.4

0.4.0.0
-------
 * Compiles with ghc 7.8

 * Fixed an issue where "P" was available as a qualified version of Prelude
   (thanks to Samuel Gélineau)

 * Uses exceptions package instead of MonadCatchIO-mtl (API breakage expected)

 * No longer depends on haskell-src

 * Changelog should now appear in Hackage

 * Integrated unit tests with cabal

0.3.3.7
-------
 * Fixed a race condition that would happen, for instance, when two process where run
   one next to the other, making them, on some platforms, to get the same random number seed
   (thanks to Mario Pastorelli and Samuel Gélineau)

 * Small fix in documentation (thanks to Daniil Frumin)

0.3.3.6
-------
 * Works again on GHC 7.2.x (thanks to Björn Peemöller)

0.3.3.5
-------
 * Works on GHC 7.4.6
 * Cleans up files for phantom modules that were left behind (thanks to Beltram Felgenhauer)

0.3.3.4
-------
 * Works on GHC 7.4.1

0.3.3.3
-------
 * Works on GHC 7.2.1

0.3.3.2
-------
 * Supports GHC 7

0.3.3.1
-------
 * Instance declaration for Applicative (InterpreterT m) works with mtl-2
   (requires Applicative m, this shouldn't break anything...)

0.3.3.0
-------
 * add unsafeRunInterpreterWithArgs
 * check that only one instance of the interpreter is run at any time

0.3.2.3
-------
 * Can be built against MonadCatchIO-mtl-0.3.x.x

0.3.2.2
-------
 * Fixed a bug that would make expressions using heavy use of the layout
   rule to fail to be interpreted (see parens)

0.3.2.1
-------
 * hint.cabal includes version bounds for package ghc-mtl. This is to
   avoid the accidental selection of the completely unrelated ghc-mtl
   internal to ghc and, apparently, installed in the hackage server

0.3.2.0
-------
 * Exports functions parens and isInterpretedModule

 * Experimental support for module annotations

 * Uses extensible-exceptions in order to provide a uniform interface
   accross different ghc versions

 * Provides an Applicative instance for IntepreterT

 * Adds an option to configurate the searchPath


0.3.1.0
-------
 * No longer uses Language.Haskell.Extension due to configuration problems with Cabal.
   Instead, it uses its own Language.Haskell.Interpreter.Extension module.

0.3.0.0
-------
 * Updated API:
   + InterpreterT monad transformer (Interpreter = InterpreterT IO)
   + No more Sessions, use runInterpreter only once
   + New options handling functions
     - but observe that there is no setOptimizations equivalent;
       since GHC does no optimization on interpreted code, this was actually
       doing nothing
 * Works with GHC 6.10 and 6.8 (untested with 6.6)

0.2.5
-----
 * setImportsQ added (modules can be imported both qualified and unqualified)

0.2.4.1
-------
 * BUGFIX: No longer fails on expressions ending in a -- comment

0.2.4
-----
 * setInstalledModsAreInScopeQualified added
 * Now depends on ghc-paths (no longer needs a custom cabal script)

0.2.2
-----
 * setOptimizations added
 * Module Language.Haskell.Interpreter.GHC.Unsafe added
   (contains unsafeSetGhcOption)
 * unit tests now based on HUnit

0.2.1
-----
 * BUGFIX: Module reloading was broken under 6.8
 * GHC.GhcExceptions are catched and turned into InterpreterErrors

0.2.0.1
-------
 * Adds the requirement cabal-version < 1.3

0.2
---

 * Works also with GHC 6.8 and 6.6
 * Added the getModuleExports function
 * withSession function throws a dynamic exception instead of returning Either Error a
 * Requires Cabal 1.2.x
