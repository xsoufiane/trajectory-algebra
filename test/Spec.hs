import System.Exit

import qualified ChrononSpec

main :: IO ()
main = do
  status <- ChrononSpec.runSpec
  if status
    then exitSuccess
    else exitFailure