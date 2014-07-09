import Build_doctests (deps)
import Test.DocTest

main :: IO ()
main = doctest $
    "-isrc"
  : "-idist/build/autogen"
  : "-optP-include"
  : "-optPdist/build/autogen/cabal_macros.h"
  : "-optP-Iinclude"
  : "-hide-all-packages"
  : map ("-package=" ++) deps ++ ["src/Text/Toml.hs"]
