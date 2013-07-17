module Data.HalfEdgeMesh where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import           Control.Monad.State

data Mesh = Mesh
    { _meshIdCounter :: Int
    , _meshVertices  :: Map Int Vertex
    , _meshEdges     :: Map Int Edge
    , _meshFaces     :: Map Int Face
    } deriving (Show)

data Vertex = Vertex
    { _vertexId       :: Int
    , _vertexPosition :: Int -- FIXME
    , _vertexEdge     :: Int
    } deriving (Show)

data Edge = Edge
    { _edgeId     :: Int
    , _edgeVertex :: Int
    , _edgeTwin   :: Int
    , _edgeNext   :: Int
    , _edgePrev   :: Int
    , _edgeFace   :: Int
    } deriving (Show)

data Face = Face
    { _faceId   :: Int
    , _faceEdge :: Int
    } deriving (Show)


mkMesh :: Mesh
mkMesh = Mesh 1 M.empty M.empty M.empty


newId :: State Mesh Int
newId = do
    counter <- liftM _meshIdCounter get
    modify $ \s -> s { _meshIdCounter = counter + 1 }
    return $ counter


createVertex :: Int -> State Mesh Int
createVertex position = do
    k <- newId
    let v = Vertex k position 0
    modify $ \s -> s { _meshVertices = M.insert k v (_meshVertices s) }
    return k

getVertex :: Int -> State Mesh Int
getVertex position = do
    vertices <- liftM _meshVertices get
    case L.find comparingPosition (M.assocs vertices) of
        Nothing -> createVertex position
        Just (k,_) -> return k

 where
    comparingPosition (k,v) = (_vertexPosition v) == position

modifyVertex :: (Vertex -> Vertex) -> Int -> State Mesh ()
modifyVertex f k = do
    modify $ \s -> s { _meshVertices = M.adjust f k (_meshVertices s) }


createEdge :: Int -> State Mesh Int
createEdge v = do
    k <- newId
    let e = Edge k v 0 0 0 0
    modify $ \s -> s { _meshEdges = M.insert k e (_meshEdges s) }
    return k

modifyEdge :: (Edge -> Edge) -> Int -> State Mesh ()
modifyEdge f k = do
    modify $ \s -> s { _meshEdges = M.adjust f k (_meshEdges s) }



addFace :: (Int, Int, Int) -> State Mesh ()
addFace (p1,p2,p3) = do
    v1 <- getVertex p1
    v2 <- getVertex p2
    v3 <- getVertex p3

    e1 <- createEdge v1
    e2 <- createEdge v2
    e3 <- createEdge v3

    modifyVertex (\v -> v { _vertexEdge = e1 }) v1
    modifyVertex (\v -> v { _vertexEdge = e2 }) v2
    modifyVertex (\v -> v { _vertexEdge = e3 }) v3

    return ()
