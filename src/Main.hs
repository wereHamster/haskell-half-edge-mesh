module Main where

import Data.HalfEdgeMesh
import qualified Data.Map as M
import Control.Monad.State

buildMesh :: State Mesh ()
buildMesh = do
    addFace [0,1,2]
    addFace [1,0,3]
    addFace [1,3,4,5]

main :: IO ()
main = do
    let mesh = execState buildMesh emptyMesh

    putStrLn "Vertices"
    forM_ (M.elems $ _meshVertices mesh) $ \x -> do
        putStrLn $ "  " ++ show (_vertexId x) ++ ": pos " ++ (show $ _vertexPosition x)

    putStrLn ""
    putStrLn "Edges"
    forM_ (M.elems $ _meshEdges mesh) $ \x -> do
        putStrLn $ "  " ++ show (_edgeId x) ++ ": vertex " ++ (show $ _edgeVertex x)

    putStrLn ""
    putStrLn "Faces"
    forM_ (M.elems $ _meshFaces mesh) $ \x -> do
        putStrLn $ "  " ++ show (_faceId x) ++ ": edge " ++ (show $ _faceEdge x)
