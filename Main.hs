module Main where
import Data.Char
import Data.Tuple
import Data.List
import Data.List.Split (splitOn)
import System.Random
import System.Console.GetOpt
import System.Environment
import System.Drawille
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.Exit
import Coloring
import Cluster
import Testing

-- Options record
data Options = Options {
   optHelp              :: Bool
 , optTest              :: Bool
 , optForceTests        :: Bool
 , optIncremental       :: Bool
 , verbosity           :: Int
 , optDisplay           :: Bool
 , scaleF               :: ([Cluster] -> [Cluster])
 , pointCount           :: Int
 , metric               :: (Point -> Center -> Double)
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optTest = False
    , optForceTests = False
    , optIncremental = False
    , verbosity = 1
    , scaleF  = id
    , pointCount = 10
    , optDisplay = False
    , metric = eucDist
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]        (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['n'] []              (ReqArg (\n opts -> opts { pointCount = (read n) }) "n") "Use n points.",
  Option ['d'] ["display"]     (NoArg  (\opts -> opts { optDisplay = True })) "Render the points and their clusters on the screen.",
  Option ['i'] ["incremental"] (NoArg  (\opts -> opts { optIncremental = True })) "Output step-by-step as the algorithm runs.",
  Option ['v'] ["verbosity"]     (ReqArg  (\n opts -> opts { verbosity = read n }) "v") "Level of information displayed during processing.",
  Option ['m'] ["metric"]      (ReqArg (\m opts -> opts { metric = (readMetric m) }) "m") ("Set the distance metric.\n" ++ showMetricOpts),
  Option ['q'] ["quiet"]       (NoArg  (\opts -> opts { verbosity = 0 })) "Only print error messages on tests, or minimal output when solving.",
  Option [] ["forceTests"]       (NoArg  (\opts -> opts { optTest=True, optForceTests = True})) "Force evaluation of all test cases, regardless of early failures.",
  Option ['t'] ["test"]        (NoArg  (\opts -> opts { optTest = True })) "Runs a series of tests on your code"
  ]

showMetricOpts :: String
showMetricOpts = "\tOptions:"
              ++ "\n\t\teuclid\t- Euclidian Distance"
              ++ "\n\t\tmanhat\t- Manhattan Distance"
              ++ "\n\t\tcheby\t- Chebyshev Distance"
              ++ "\n\t\ttraffic\t- Traffic Distance"
              ++ "\n\t\tregion\t- Township Distance"

readMetric :: String -> (Point -> Center -> Double)
readMetric "euclid" = eucDist
readMetric "manhat" = manhatDist
readMetric "cheby"  = chebyDist
readMetric "traffic" = trafficDist
readMetric "region" = townshipDist
readMetric s = error "Invalid metric"

-- Return the list of flags
compilerOpts :: [String] -> (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: cluster [OPTION]... [HAND]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./cluster [OPTION]... [k] [file]"

-- Main IO function
main :: IO ()
main = do
  allArgs <- getArgs
  let (opts, args) = compilerOpts allArgs
  when (length args > 2) $ do
         putStrLn "Error: more than two arguments. Ignoring all but the first two."
  case (optHelp opts, optTest opts, args) of
    (True, _,_) -> helpIO
    (_, True,_) -> runTests (verbosity opts) (optForceTests opts) 
    (_,_,[])    -> do
                     points <- getKRandomPoints (pointCount opts)
                     playKMeans opts 2 points
    (_,_,[k])   -> do
                    points <- getKRandomPoints (pointCount opts)
                    playKMeans opts (read k) points
    (_,_,[k,fl])-> do
                    points <- fmap fst $ pointsOfFile (pointCount opts) fl
                    playKMeans opts {scaleF=scaleClusters} (read k) points
    _ -> error "Improper arguments. Usage: ./cluster [OPTION]... [k] [file]"


-- kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
playKMeans :: Options -> Int -> [Point] -> IO ()
playKMeans opts k points =
  if optIncremental opts
    then playIncrementalKMeans opts k points
    else do
      let clusters = kMeans (metric opts) k points
      if (optDisplay opts)
        then (showClustering . scaleF opts) clusters
        else basicDisplay opts clusters

-- one step at a time
playIncrementalKMeans :: Options -> Int -> [Point] -> IO ()
playIncrementalKMeans opts k points = 
  let initCenters = getKElems k (nub points)
      initClusters = assignPoints (metric opts) initCenters points
      showFunc = if (optDisplay opts) then (showClustering . scaleF opts) else basicDisplay opts
      iterFunc = improveClusters (metric opts) k
      increment i clusters = do
        putMagentaLn $ "Iterations: " ++ (show i)
        showFunc clusters
        putMagentaLn $ "Hit \"Enter\" to proceed, or type \"q\" to quit."
        input <- getLine
        case input of
            "q" -> putGreenLn "Exiting!" >> exitSuccess
            _   -> increment (i+1) (iterFunc clusters)
  in increment 0 initClusters
  

basicDisplay :: Options -> [Cluster] -> IO ()
basicDisplay opts clusters 
  | verbosity opts == 0 = putStr $ unwords $ sort $ map showCluster $ scaleF opts clusters
  | otherwise = aux 1 clusters
  where
    showCluster ((x,y,l),ps) =
      let x' = (fromInteger $ round $ x * (10^4)) / (10.0^^4)
          y' = (fromInteger $ round $ y * (10^4)) / (10.0^^4)
      in  show (x', y', length ps)
    showPoint (x,y,l) =
      let x' = (fromInteger $ round $ x * (10^2)) / (10.0^^2)
          y' = (fromInteger $ round $ y * (10^2)) / (10.0^^2)
      in  show (x', y')
    aux n ((c, ps):xs) = do
        putStr $ "Center " ++ (show n) ++ " " ++ showPoint c ++ " covering " ++ (show (length ps)) ++ " points"
        when (verbosity opts == 2) $ putStr $ ": "++ (unwords $ map showPoint ps)
        putStrLn "."
        aux (n + 1) xs
    aux _ [] = return ()

getKRandomPoints :: Int -> IO [Point]
getKRandomPoints k = do
  ps <- replicateM (k * 2) $ randomRIO(0.0,10.0) :: IO [Double]
  let rounded = map (\x -> fromIntegral (round (x * 100)) / 100.0) $ ps
  let pairs = pair 0 rounded
  return pairs
  where
    pair n (x:y:xs) = (x,y,n):(pair (n + 100) xs)
    pair _ _ = []

-- (Center, [Point])
showClustering :: [Cluster] -> IO ()
showClustering c = do
  let centers = highlight $ map (\(a, b, _) -> (round (a * 10), round (b * 10))) $ map fst c
      points  = map (\(a, b, _) -> (round (a * 10), round (b * 10))) $ foldl (++) [] $ map snd c
      plot    = frame $ fromList $ centers ++ points
  putWhiteLn $ plot

-- create visual clusters by expanding their points into squares
highlight :: [(Int, Int)] -> [(Int, Int)]
highlight (p@(x, y):xs) = p : (x - 1, y) : (x, y - 1) : (x + 1, y) : (x, y + 1) : (x + 1, y + 1) : (x + 1, y - 1) : (x - 1, y + 1) : (x - 1, y - 1) : highlight xs
highlight [] = []

--helper functions
readPoint line labels =
    let (x:y:rest) = splitOn "," line
        (labelVal, label) = makeLabel rest labels
        point = (read x, read y, label)
        newLabels = nub ((labelVal, label):labels)
    in (point, newLabels)

freshLabel labels = 1+(maximum $ 0:(map snd labels))

makeLabel rest labels = 
    let x = case length rest of
                0 -> show (freshLabel labels)
                1 -> head rest
                5 -> rest!!3
                k -> unwords rest
    in case lookup x labels of
        Nothing -> (x, freshLabel labels)
        Just y -> (x,y)

--readPoints :: [String] -> ([Point],[(Int, String)])
readPoints strs = (points, map swap assocs)
    where readFold (pnts,lbls) s = let (p,lbls') = readPoint s lbls in (p:pnts, lbls')
          (points, assocs) =  foldl readFold ([],[]) strs

pointsOfFile n fname = do
    contents <- readFile fname
    return $ readPoints $ take n $ lines contents

range lst = (minimum lst, maximum lst)

--squarePoints :: [Point] -> [Point]
squarePoints points =
    let (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        xdif = xmax - xmin
        ydif = ymax - ymin
        xScale xval = (10*(xval-xmin))/xdif
        yScale yval = (10*(yval-ymin))/ydif
    in map (\(x,y,l) -> (xScale x, yScale y, l)) points

--scalePoints :: [Point] -> [Point]
scalePoints points =
    let (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        factor = min (10/(xmax-xmin)) (10/(ymax-ymin))
        xScale xval = factor * (xval-xmin)
        yScale yval = factor * (yval-ymin)
    in map (\(x,y,l) -> (xScale x, yScale y, l)) points

--scaleClusters :: [Cluster] -> [Cluster]
scaleClusters clusters =
    let points = concatMap snd clusters
        (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        factor = min (10/(xmax-xmin)) (10/(ymax-ymin))
        xScale xval = factor * (xval-xmin)
        yScale yval = factor * (yval-ymin)
        scalePoint (x,y,l) = (xScale x, yScale y, l)
        scaleCluster (center,cpoints) = (scalePoint center, map scalePoint cpoints) 
    in map scaleCluster clusters 
