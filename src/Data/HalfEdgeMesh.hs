module Data.HalfEdgeMesh where

import System.IO.Unsafe

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import           Control.Monad.State
import           Control.Applicative

type Position = Int

data Mesh = Mesh
    { _meshIdCounter :: Int
    , _meshVertices  :: Map Int Vertex
    , _meshEdges     :: Map Int Edge
    , _meshFaces     :: Map Int Face
    } deriving (Show)

data Vertex = Vertex
    { _vertexId       :: Int
    , _vertexPosition :: Position -- FIXME
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


emptyMesh :: Mesh
emptyMesh = Mesh 1 M.empty M.empty M.empty


newId :: State Mesh Int
newId = do
    counter <- liftM _meshIdCounter get
    modify $ \s -> s { _meshIdCounter = counter + 1 }
    return $ counter


createVertex :: Position -> State Mesh Int
createVertex position = do
    k <- newId
    let v = Vertex k position 0
    modify $ \s -> s { _meshVertices = M.insert k v (_meshVertices s) }
    return k

getVertex :: Position -> State Mesh Int
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

getEdge :: Int -> State Mesh Edge
getEdge e = do
    edges <- liftM _meshEdges get
    maybe (bug edges) return $ M.lookup e edges
    --return $ fromJust $ M.lookup e edges

  where
    bug edges = error $ "Can't find edge with id " ++ (show e)

withEdge :: Int -> (Edge -> State Mesh ()) -> State Mesh ()
withEdge e f = do
    edges <- liftM _meshEdges get
    case M.lookup e edges of
        Nothing -> return ()
        Just x -> f x

findEdge :: Int -> Int -> State Mesh Int
findEdge v0 v1 = do
    edges <- liftM _meshEdges get
    case L.find comparingVertices (M.assocs edges) of
        Nothing -> return 0
        Just (k,_) -> return k

 where
    comparingVertices (k,e) = (k,(_edgeNext e)) == (v0,v1)

createEdge :: Int -> State Mesh Int
createEdge v = do
    k <- newId
    let e = Edge k v 0 0 0 0
    modify $ \s -> s { _meshEdges = M.insert k e (_meshEdges s) }
    return k

modifyEdge :: (Edge -> Edge) -> Int -> State Mesh ()
modifyEdge f k = do
    modify $ \s -> s { _meshEdges = M.adjust f k (_meshEdges s) }

setNextEdge :: Int -> Int -> State Mesh ()
setNextEdge e0 e1 = modifyEdge (\e -> e { _edgeNext = e1 }) e0

setPreviousEdge :: Int -> Int -> State Mesh ()
setPreviousEdge e0 e1 = modifyEdge (\e -> e { _edgePrev = e1 }) e0

createEdges :: [Int] -> State Mesh Int
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

createFace :: Int -> State Mesh Int
createFace e = do
    k <- newId
    let f = Face k e
    modify $ \s -> s { _meshFaces = M.insert k f (_meshFaces s) }
    return k

getFace :: Int -> State Mesh Face
getFace f = do
    faces <- liftM _meshFaces get
    maybe bug return (M.lookup f faces)

  where
    bug = error $ "Can't find face with id " ++ (show f)

forEachEdge :: Int -> (Int -> State Mesh ()) -> State Mesh ()
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
