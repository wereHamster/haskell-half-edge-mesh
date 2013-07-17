module Main where

import Data.HalfEdgeMesh
import Control.Monad.State

p1 = 0
p2 = 3
p3 = 8

main :: IO ()
main = do
    let mesh = execState (addFace (p1,p2,p3)) mkMesh
    putStrLn $ show (_meshVertices mesh)
    let mesh1 = execState (addFace (p1,p2,9)) mesh
    putStrLn $ show (_meshVertices mesh1)
