module Data.HalfEdgeMesh where

import System.IO.Unsafe

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import           Control.Monad.State
import           Control.Applicative

type Position = Int

type VertexId = Int
type EdgeId   = Int
type FaceId   = Int

data Mesh = Mesh
    { _meshIdCounter :: Int
    , _meshVertices  :: Map VertexId Vertex
    , _meshEdges     :: Map EdgeId Edge
    , _meshFaces     :: Map FaceId Face
    } deriving (Show)

data Vertex = Vertex
    { _vertexId       :: VertexId
    , _vertexPosition :: Position -- FIXME
    , _vertexEdge     :: Int
    } deriving (Show)

data Edge = Edge
    { _edgeId     :: EdgeId
    , _edgeVertex :: VertexId
    , _edgeTwin   :: EdgeId
    , _edgeNext   :: EdgeId
    , _edgePrev   :: EdgeId
    , _edgeFace   :: FaceId
    } deriving (Show)

data Face = Face
    { _faceId   :: FaceId
    , _faceEdge :: EdgeId
    } deriving (Show)


emptyMesh :: Mesh
emptyMesh = Mesh 1 M.empty M.empty M.empty


newId :: State Mesh Int
newId = do
    counter <- liftM _meshIdCounter get
    modify $ \s -> s { _meshIdCounter = counter + 1 }
    return $ counter


createVertex :: Position -> State Mesh VertexId
createVertex position = do
    k <- newId
    let v = Vertex k position 0
    modify $ \s -> s { _meshVertices = M.insert k v (_meshVertices s) }
    return k

getVertex :: Position -> State Mesh VertexId
getVertex position = do
    vertices <- liftM _meshVertices get
    case L.find comparingPosition (M.assocs vertices) of
        Nothing -> createVertex position
        Just (k,_) -> return k

 where
    comparingPosition (k,v) = (_vertexPosition v) == position

modifyVertex :: (Vertex -> Vertex) -> VertexId -> State Mesh ()
modifyVertex f k = do
    modify $ \s -> s { _meshVertices = M.adjust f k (_meshVertices s) }

getEdge :: EdgeId -> State Mesh Edge
getEdge e = do
    edges <- liftM _meshEdges get
    maybe (bug edges) return $ M.lookup e edges
    --return $ fromJust $ M.lookup e edges

  where
    bug edges = error $ "Can't find edge with id " ++ (show e)

withEdge :: EdgeId -> (Edge -> State Mesh ()) -> State Mesh ()
withEdge e f = do
    edges <- liftM _meshEdges get
    case M.lookup e edges of
        Nothing -> return ()
        Just x -> f x

findEdge :: VertexId -> VertexId -> State Mesh EdgeId
findEdge v0 v1 = do
    edges <- liftM _meshEdges get
    case L.find comparingVertices (M.assocs edges) of
        Nothing -> return 0
        Just (k,_) -> return k

 where
    comparingVertices (k,e) = (k,(_edgeNext e)) == (v0,v1)

createEdge :: VertexId -> State Mesh EdgeId
createEdge v = do
    k <- newId
    let e = Edge k v 0 0 0 0
    modify $ \s -> s { _meshEdges = M.insert k e (_meshEdges s) }
    return k

modifyEdge :: (Edge -> Edge) -> EdgeId -> State Mesh ()
modifyEdge f k = do
    modify $ \s -> s { _meshEdges = M.adjust f k (_meshEdges s) }

setNextEdge :: EdgeId -> EdgeId -> State Mesh ()
setNextEdge e0 e1 = modifyEdge (\e -> e { _edgeNext = e1 }) e0

setPreviousEdge :: EdgeId -> EdgeId -> State Mesh ()
setPreviousEdge e0 e1 = modifyEdge (\e -> e { _edgePrev = e1 }) e0

createEdges :: [VertexId] -> State Mesh EdgeId
createEdges vertices = do
    edges <- mapM createEdge vertices
    go (head edges) edges

  where
    go first [last] = do
        setNextEdge last first
        setPreviousEdge first last
        return first

    go first (e1:e2:rest) = do
        setNextEdge e1 e2
        setPreviousEdge e2 e1
        go first (e2:rest)

createFace :: EdgeId -> State Mesh FaceId
createFace e = do
    k <- newId
    let f = Face k e
    modify $ \s -> s { _meshFaces = M.insert k f (_meshFaces s) }
    return k

getFace :: FaceId -> State Mesh Face
getFace f = do
    faces <- liftM _meshFaces get
    maybe bug return (M.lookup f faces)

  where
    bug = error $ "Can't find face with id " ++ (show f)

forEachEdge :: FaceId -> (EdgeId -> State Mesh ()) -> State Mesh ()
forEachEdge fid f = do
    face <- getFace fid
    go (_faceEdge face) (_faceEdge face)

  where
    go e0 e1 = do
        f e1
        edge <- getEdge e1
        if (_edgeNext edge) == e0
            then return ()
            else go e0 (_edgeNext edge)



addFace :: [Position] -> State Mesh ()
addFace positions = do
    vertices <- mapM getVertex positions
    firstEdge <- createEdges vertices
    face <- createFace firstEdge

    -- Update twins for all edges.
    forEachEdge face $ \e -> do
        edge <- getEdge e
        twin <- findEdge (_edgeNext edge) e
        modifyEdge (\x -> x { _edgeTwin = twin }) e


buildMesh :: State Mesh a -> Mesh
buildMesh f = execState f emptyMesh

numVertices :: Mesh -> Int
numVertices = M.size . _meshVertices

numEdges :: Mesh -> Int
numEdges = M.size . _meshEdges

numFaces :: Mesh -> Int
numFaces = M.size . _meshFaces

vertices :: Mesh -> [ Vertex ]
vertices = M.elems . _meshVertices

outgoingEdges :: VertexId -> Mesh -> [ Edge ]
outgoingEdges v mesh = []
  where
    vertex = fromJust $ M.lookup v (_meshVertices mesh)
