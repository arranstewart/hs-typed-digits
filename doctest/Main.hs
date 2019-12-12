{-# LANGUAGE CPP #-}

import Test.DocTest
import System.FilePath.Glob

-- doctest >= 0.15.0 adds useful "--verbose" flag
#if MIN_VERSION_doctest(0,15,0)
hasVerbose = True
#else
hasVerbose = False
#endif

main = do
  srcFiles <- globDir1 (compile "**/*.hs") "src"
  let doctestOpts = if hasVerbose
                    then ["-isrc", "--verbose"]
                    else ["-isrc"]
  doctest $ doctestOpts ++ srcFiles

