import Test.Tasty

import qualified Relation.RelationSpec as RelationSpec

-----------------------------------

main :: IO ()
main = defaultMain $ testGroup "Trajectory Algebra Spec"
    [ RelationSpec.spec
    ]
    
