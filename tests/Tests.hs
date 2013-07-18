module Main where

import Test.Hspec
import Data.HalfEdgeMesh
import Control.Exception (evaluate)


main :: IO ()
main = hspec spec


checkSize :: String -> Mesh -> (Int,Int,Int) -> Spec
checkSize label mesh (v,e,f) = describe label $ do
    it ("should have " ++ (show v) ++ " vertices") $ shouldBe (numVertices mesh) v
    it ("should have " ++ (show e) ++ " edges")    $ shouldBe (numEdges mesh) e
    it ("should have " ++ (show f) ++ " face")     $ shouldBe (numFaces mesh) f

singleFaceMesh = buildMesh (addFace [1,2,3])
doubleFaceMesh = buildMesh (addFace [0,1,2] >> addFace [0,2,3])

spec :: Spec
spec = do
    describe "basic size tests" $ do
        checkSize "empty mesh" emptyMesh (0,0,0)
        checkSize "mesh with a single face" singleFaceMesh (3,3,1)
        checkSize "mesh with two adjacent faces" doubleFaceMesh (4,6,2)

    describe "outgoingEdges" $ do
        it "should return an empty list if the vertex doens't exist" $ do
            length (outgoingEdges 0 emptyMesh) `shouldBe` 0
        it "should return one edge if the vertex has only one edge" $ do
            length (outgoingEdges 1 singleFaceMesh) `shouldBe` 1
