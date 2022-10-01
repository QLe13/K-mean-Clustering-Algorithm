module Cluster where
import Data.List
import Data.Maybe
import Debug.Trace

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

dataAt :: Int -> [a] -> a
dataAt _ [] = error "Empty List!"
dataAt y (x:xs)  | y <= 0 = x
                 | otherwise = dataAt (y-1) xs

helper :: [Int] -> [a] -> [a]
helper l1 l2 = [dataAt x l2 | x <- l1]

anotherHelper :: Double -> Double -> Double -> [Int]
anotherHelper end inc start = if start > end then [] else [ (floor start)::Int ]++ anotherHelper end inc (start+inc)

getKElems :: Int -> [a] -> [a]
getKElems 0 _ = []
getKElems k ls = 
       let len = fromIntegral(length ls -1)
           increment = len/(fromIntegral(k-1))
        in helper (anotherHelper len increment 0) ls 


--Example: getKElems 3 [1..6] = [1,3,6]

--Return the Euclidean distance between two points. 

eucDist :: Point -> Point -> Double
eucDist (x,y,z) (a,b,c) = sqrt ((x-a)^2+(y-b)^2)
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist (x,y,z) (a,b,c) = abs(x-a) + abs(y-b)
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist (x,y,z) (a,b,c) = maximum[abs(x-a),abs(y-b)]
--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist (x,y,z) (a,b,c) = 2*abs(x-a) + abs(y-b)
--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist (x,y,z) (a,b,c) 
                        | z == c = abs(x-a) + abs(y-b)
                        | otherwise = (abs(x-a) + abs(y-b)) * 2
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average list = (sum list) / (fromIntegral (length list))
--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.

minimize :: (a -> Double) -> [a] -> a
minimize func lst = head[x | x <- lst, func x  == minimum( map func lst)]
--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets, 
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).

bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket func [] l2 = []
bucket func (x:xs) l2 = (x, [y | y <- l2, func y ==x]):bucket func xs l2
 
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]

--Given a metric, a list of centers, and a point, return the center closest to the point.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint met cens p = minimize (met p) cens
    
    
--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is assigned to the nearest center.

assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints met cens ps = bucket (assignPoint met cens) cens ps
--Examples:
--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),(2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


--Given a metric and a cluster, return the mean of the points in the cluster.
get3rd (_,_,a) = a 
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean met (_,[]) = Nothing
findMean met (x,ps) = let
                    a = average[ x | (x,_,_) <- ps]
                    b = average[ y | (_,y,_) <- ps]
                    label = minimize (met (a,b,-1)) ps
                    in Just ( a, b , abs (get3rd label))

                    
--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, relocate all the centers to the mean of their clusters.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters met clus =
    let vcluster = [ x | x <- clus, snd x /= []]
    in [fromMaybe (0,0,0 ) (findMean met y) | y <- vcluster]

--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters met k clus = 
                let newCen = moveCenters met clus
                    bigClus = snd (maximum[(length xs, xs) |(x,xs)<- clus])
                    non0cens = getKElems (k-length newCen) bigClus
                    genCen = newCen ++ non0cens
                    points = concat [ snd x | x <- clus]
                    in assignPoints met genCen points

--Example: let newClusters = improveClusters trafficDist 2 testClusters 
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
repImprove :: Int -> (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
repImprove 0 met k clus = clus
repImprove int met k clus = let 
                newClus = improveClusters met k clus
                in if newClus == clus then newClus else repImprove (int-1) met k newClus

kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans met k ps = let
                initClus = assignPoints met (getKElems k (nub ps)) ps
                in repImprove iterationLimit met k initClus

