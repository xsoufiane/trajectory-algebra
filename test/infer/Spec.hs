import QuickSpec

import qualified ChrononSpec

--------------------------------------

main :: IO ()
main = do
  quickSpec ChrononSpec.sig
    