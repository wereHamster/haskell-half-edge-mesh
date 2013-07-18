module Main where

import Test.Hspec
import Data.HalfEdgeMesh

main :: IO ()
main = hspec spec


checkSize :: Mesh -> (Int,Int,Int) -> Spec
checkSize mesh (v,e,f) = do
    it ("should have " ++ (show v) ++ " vertices") $ shouldBe (numVertices mesh) v
    it ("should have " ++ (show e) ++ " edges")    $ shouldBe (numEdges mesh) e
    it ("should have " ++ (show f) ++ " face")     $ shouldBe (numFaces mesh) f

spec :: Spec
spec = do
    describe "empty mesh" $ do
        let mesh = emptyMesh
        checkSize mesh (0,0,0)


    describe "simple mesh" $ do
        let mesh = buildMesh $ addFace [0,1,2]
        checkSize mesh (3,3,1)


    describe "complex mesh" $ do
        let mesh = buildMesh $ do
            addFace [0,1,2]
            addFace [0,2,3]

        checkSize mesh (4,6,2)
