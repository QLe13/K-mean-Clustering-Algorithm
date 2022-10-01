module Testing where
--todo: truncate numbers.
import Data.List
import Data.Semigroup
import Cluster
import TestCases
import Test.Grader.Tests
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Rubric
import Control.Monad.Extra
import Control.Monad.Trans.RWS


sortClusters clusters = 
    sort $ map (\(a,b) -> (a,sort b)) clusters

testGetKElems :: Grader String
testGetKElems = assess "getKElems" 2 $ do
        check "that getKElems always returns k elems" $ 
          foldl1 (<>) [ (k, to n, length $ getKElems k [0..n]) `shouldBe` (k, to n, k) | (k,n) <- combinations]
        check "that getKElems does not choose any duplicates" $ getKElems 3 [1..10] `shouldSatisfy` ((==3) . length . nub)
        check "that getKElems always returns elements from the list" $ 
            getKElems 3 lst1 `containsOnly` lst1 
            <> getKElems 5 lst2 `containsOnly` lst2
        where lst1 = "AZ#*"
              lst2 = [1..100]
              combinations =[(1,5), (2,5), (3,5), (5,5), (5,10), (10, 21)] 
              to n = "[0.."++show n++"]"


testEucDist :: Grader String
testEucDist = assess "eucDist" 3 $ do
        check "that (0,0) to (1,1) is less than (0,0), to (0,2)" $ eucDist (0,0,10) (1,1,10) `shouldSatisfy` (< eucDist (0,0,10) (0,2,10))
        check "that (0,0) to (0,0) is distance 0" $ (eucDist (0,0,10) (0,0,10)) `shouldBe` 0.0
        check "that order and township doesn't change distance" $ (eucDist (10,5,10) (5,10,10)) `shouldBe` (eucDist (5,10,00) (10,5,10)) 
        check "distance along the x axis" $ (eucDist (0,0,10) (0,5.5,10)) `shouldBeIn` [5.5, 30.25]
        check "distance from (0,0) to (5.34, 5.28)" $ round (eucDist (0,0,10) (5.34,5.28,10)) `shouldBeIn` [56, 8] 
        check  "that distance from (5,5) to (0,0) is positive" ((eucDist (5,5,10) (0,0,10) `shouldSatisfy` (> 0))) 

testChebyDist :: Grader String
testChebyDist = assess "chebyDist" 3 $ do
        check "that (0,0) has the same distance to (0,5) and (5,5)" $ chebyDist (0,0,10) (0,5,10) `shouldBe` chebyDist (0,0,10) (5,5,10)
        check "distance from (15,0.5) to (50,23)" $ chebyDist (15,0.5,10) (50,23,10) `shouldBe` 35.0
        check "distance from (0,0) to (55,0)" $ chebyDist (0,0,10) (55,0,10) `shouldBe` 55.0
        check "distance from (15,15) to (15,15)" $ chebyDist (20,20,10) (55,0,10) `shouldBe` 35.0
        check "distance from (0,0) to (5.34, 5.28)" $ chebyDist (15,15,10) (15,15,10) `shouldBe` 0
        check  "distance from (0,15) to (15,0)" $ chebyDist (0,15,10) (15,0,10) `shouldBe` 15.0
        check "distance from (0, 10) to (5, 0)" $ chebyDist (0, 10, 10) (5,0,10) `shouldBe` 10
        check "distance from (0,5) to (10,0)" $ chebyDist (0,5,10) (10,0,10) `shouldBe` 10

testTrafficDist :: Grader String
testTrafficDist = assess "trafficDist" 3 $ do
        check "that (0,0) has the same distance to (0,10) and (5,0)" $ trafficDist (0,0,10) (0,10,10) `shouldBe` trafficDist (0,0,10) (5,0,10)
        check "distance from (0,0) to (0,10)" $ trafficDist (0,0,10) (0,10,10) `shouldBe` 10.0
        check "distance from (0,0) to (10,00)" $ trafficDist (0,0,10) (10,0,10) `shouldBe` 20.0
        check "distance from (0,0) to (5.5,5.5)" $ trafficDist (0,0,10) (5.5,5.5,10) `shouldBe`  16.5
        check "distance from (0,0) to (5,4)" $ trafficDist (0,0,10) (5,4,10) `shouldBe` 14.0
        check "distance from (0,5) to (5,0)" $ trafficDist (0,5,10) (5,0,10) `shouldBe` 15.0
        check "distances are positive" $ trafficDist (5,5,10) (0,0,10) `shouldSatisfy` (> 0)

testTownshipDist :: Grader String
testTownshipDist = assess "townshipDist" 3 $ do
        check "that distance from (0,0) to (1,1) doubles across townships" $ townshipDist (0,0,10) (1,1,20) `shouldBe` (2 * (townshipDist (0,0,10) (1,1,10)))
        check "distance from (0,0) to (0,10) within a township" $ townshipDist (0,0,10) (0,10,10) `shouldBe` 10.0
        check "distance from (0,0) to (10,0) between townships" $ townshipDist (0,0,10) (10,0,20) `shouldBe` 20.0
        check "distance from (0,0) to (5.5,5.5) within a township" $ townshipDist (0,0,10) (5.5,5.5,10) `shouldBe` 11.0
        check "distance from (0,0) to (4,4) between townships" $ townshipDist (0,0,10) (5,4,20) `shouldBe` 18.0
        check "distance from (0,5) to (5,0) within a township" $ townshipDist (0,5,10) (5,0,10) `shouldBe` 10.0
        check "distances are positive" $ townshipDist (5,5,10) (0,0,20) `shouldSatisfy` (> 0)

testAverage :: Grader String
testAverage = assess "average" 3 $ do
        check "the average of [0,5,10]" $ average [0, 5, 10] `shouldBe` 5
        check "the average of [1..10]++[10,10,10]" $ (average $ [1..10] ++ [10,10,10]) `shouldApprox` 6.54

testMinimize :: Grader String
testMinimize = assess "minimize" 6 $ do
        check "minimize (fromIntegral . length) [\"aaaa\",\"b\"]" $ (minimize (fromIntegral . length) ["aaaa", "b"]) `shouldBe` "b"
        check "minimize abs [-50..50]" $ (minimize (abs) [-50..50]) `shouldBe` 0.0 
        check "minimize id [5,7,10,-3,-10,4,-20]" $ (minimize (id) [5, 7, 10, -3, -10, 4, -20]) `shouldBe` (-20)
        check "that minimize (fromIntegral.length) [\"a\",\"c\",\"aa\"] returns a 1-length string" $ 
                length (minimize (fromIntegral . length) ["a", "c", "aa"]) `shouldBe` 1

testBucket :: Grader String
testBucket = assess "bucket" 6 $ do
        check "bucket length [1..3] [\"Hi\",\"my\",\"job\",\"is\",\"fun\",\"!\"]" $ 
                (sort $ bucket length [1..3] ["Hi","my","job","is","fun","!"]) `shouldBe` [(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
        check "bucket on empty lists" $ (bucket length [] []) `shouldBe` ([] :: [(Int,[String])])
        check "bucket with buckets [1..3], but no elements" $ (sort $ bucket length [1..3] []) `shouldBe` [(1,([]::[String])),(2,[]),(3,[])]
                
testAssignPoint :: Grader String
testAssignPoint = assess "assignPoint" 5 $ do
        check "assignPoint eucDist [(0,0,-1),(5,7,-1)] (5,0,100)" $ (assignPoint eucDist [(0,0,-1),(5,7,-1)] (5,0,100)) `shouldBe` (0.0,0.0,-1)
        check "assignPoint trafficDist [(0,0,-1),(5,7,-1)] (5,0,100)" $ (assignPoint trafficDist [(0,0,-1),(5,7,-1)] (5,0,100)) `shouldBe` (5.0,7.0,-1)
        check "assignPoint manhatDist [(0,0,100),(2,0,200)] (5,0,100)" $ (assignPoint manhatDist [(0,0,100),(2,0,200)] (5,0,100)) `shouldBe` (2.0,0.0,200)
        check "assignPoint townshipDist [(0,0,100),(2,0,200)] (5,0,100)" $ (assignPoint townshipDist [(0,0,100),(2,0,200)] (5,0,100)) `shouldBe` (0.0,0.0,100)
        check "assignPoint eucDist [(0,0,3), (10,10,4), (2,2,1)] (4,4,-1)" $ (assignPoint eucDist [(0,0,3), (10,10,4), (2,2,1)] (4,4,-1)) `shouldBe` (2,2,1)

testAssignPoints :: Grader String
testAssignPoints = assess "assignPoints" 5 $ do 
        check "assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints" $ 
                (smry $ assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints) `shouldBe` [((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
        check "assignPoints trafficDist closeCenters closePoints" $ 
                (sortClusters $ assignPoints trafficDist closeCenters closePoints) `shouldBe` sortClusters closeAnswer
        check "assignPoints trafficDist closeCentersEmpty closePoints" $ 
                (sortClusters $ assignPoints trafficDist closeCentersEmpty closePoints) `shouldBe` sortClusters closeAnswerEmpty
        where smry clusters = [(c, length ps) | (c,ps) <- sortClusters clusters]



testFindMean :: Grader String
testFindMean = assess "findMean" 8 $ do
        check "findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)])" $ 
                (findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)])) `shouldBe` (Just (4.0,4.0,1))
        check "findMean townshipDist ((3,3,0), [(0,0,0), (9,9,0), (3,3,1)])" $ 
                (findMean townshipDist ((3,3,0), [(0,0,0), (9,9,0), (3,3,1)])) `shouldBe` (Just (4.0,4.0,1))
        check "findMean chebyDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])" $
                (findMean chebyDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])) `shouldBe` (Just (5.0,5.0,0))
        check "findMean eucDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])" $ 
                (findMean eucDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])) `shouldBe` (Just (5.0,5.0,1))
        check "findMean townshipDist ((3,3,0), [])" $ (findMean townshipDist ((3,3,0), [])) `shouldBe` Nothing
        naive <- check "findMean townshipDist fmNaiveCase" $ 
                (findMean townshipDist fmNaiveCase) `shouldBe` (Just (4.0,4.0,1))
        reasonable <- check "findMean townshipDist on fmZeroCase and fmOneCase" $ 
                (findMean townshipDist fmZeroCase) `shouldBe` (Just (4.0,4.0,1))
             <> (findMean townshipDist fmOneCase) `shouldBe` (Just (4.0,4.0,2))
        excellent <- check "findMean townshipDist fmNegCase" $ 
                (findMean townshipDist fmNegCase) `shouldBe` (Just (4.0,4.0,1))
        return $ do
          whenM allPassed $ passWith 8
          whenM (failedOnly [excellent]) $ passWith 7
          whenM (failedOnly [reasonable]) $ passWith 7
          whenM (failedOnly [naive]) $ passWith 6
          whenM (failedOnly [reasonable, excellent]) $ passWith 6
          whenM (failedOnly [naive, reasonable, excellent]) $ passWith 5
          whenM (failedOnly [naive, excellent]) $ passWith 5
          whenM (failedOnly [naive, reasonable]) $ passWith 5


testMoveCenters :: Grader String
testMoveCenters = assess "moveCenters" 7 $ do
        check "moveCenters trafficDist moveCenterCase1" $ (moveCenters trafficDist testAnswer) `shouldBe` [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]
        check "moveCenters trafficDist moveCenterCase2" $ (moveCenters trafficDist closeAnswer) `shouldBeIn` closeMoved
        where closeMoved = [[(1.0666666666666667,1.0666666666666667,300),(6.742857142857142,7.071428571428571,900)]
                           ,[(1.0666666666666667,1.0666666666666667,300),(6.742857142857143,7.071428571428571,900)]]


kMeansProperties dist k = 
    let clusters = kMeans dist k tenPoints
        centersAreMeans ((x,y,l), points) = 
            let xs = map (\(x,_,_) -> x) points
                ys = map (\(_,y,_) -> y) points
            in x*(fromIntegral $ length xs) == sum xs &&  y * (fromIntegral $ length xs)  == sum ys
        centers = map fst clusters
        pointDists = concatMap (\(c,ps) -> map (\p -> (p,dist p c)) ps) clusters
        smallestDist (p,d) = all (\c -> dist p c >= d) centers
    in all centersAreMeans clusters && all smallestDist pointDists

testImproveClusters :: Grader String
testImproveClusters = assess "improveClusters" 10 $ do
        check "improveClusters trafficDist 2 improveCase1" $ 
                [(c, length ps) | (c,ps) <- newClusters] `shouldBe` [((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]
        -- check "that the points found by improveClusters are 1) the closest and 2) actually means" $ 
        --         improveClusters trafficDist 6 sTestAnswer `shouldSatisfy` 
        where  newClusters = improveClusters trafficDist 2 testAnswer

testKMeans :: Grader String
testKMeans = assess "kMeans" 10 $ do
        check "kMeans eucDist 2 tenPoints" $ (sortClusters $ kMeans eucDist 2 tenPoints) `shouldBe` correctRes
        check "kMeans manhatDist 3 tenPoints" $ 3 `shouldSatisfy` kMeansProperties manhatDist
        check "kMeans chebyDist 5 tenPoints" $ 5 `shouldSatisfy` kMeansProperties chebyDist
        check "kMeans trafficDist 7 tenPoints" $ 7 `shouldSatisfy` kMeansProperties trafficDist
        where correctRes = sortClusters $ [((7.5,2.1,400),sort [(7.0,1.0,100),(8.0,1.0,300),(8.0,2.0,400),(7.0,3.0,200),(7.5,3.5,500)])
                         ,((2.0,7.0,600),[(2.0,7.0,600),(1.0,7.0,700),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]

testReplaceMissingClusters :: Grader String
testReplaceMissingClusters = assess "full credit: that you replace missing clusters" 8 $ do
        replaced <- check "that a missing cluster gets replaced" $ length impClusters3 `shouldBe` 3
        largest <- check "that the largest cluster is split" $ length (filter upperRight impClusters3) `shouldBe` 2
        notBroken <- check "that points are preserved/don't change" $ upperRightPoints `shouldBe` (sort $ snd $ clusters !! 1)
        return $ do 
                whenM allPassed (passWith 8)
                whenM (passedExactly [replaced, notBroken]) (passWith 4)
                whenM (passedExactly [replaced]) (passWith 2)
        where clusters = [((1.0,1.0,100),[(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400)]),
                ((7.0,7.0,200),[(7.0,7.1,200),(7.5,7.5,500),(6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)])]
              impClusters3 = improveClusters eucDist 3 clusters
              upperRight ((x,y,l), ps) = x >= 6 && y >= 6
              upperRightPoints = sort $ concatMap snd (filter upperRight impClusters3)
              correctRes = sortClusters $ [((7.5,2.1,400),sort [(7.0,1.0,100),(8.0,1.0,300),(8.0,2.0,400),(7.0,3.0,200),(7.5,3.5,500)])
                                ,((2.0,7.0,600),[(2.0,7.0,600),(1.0,7.0,700),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]

testGetKElemsFC :: Grader String
testGetKElemsFC = assess "full credit: getKElems samples evenly" 7 $ do
        spread <- check "that chosen elements are spread out at least some" $ 
                       (gaps $ getKElems 3 [0..19]) `shouldSatisfy` (all (>1))
                    <> (gaps $ getKElems 5 [0..19]) `shouldSatisfy` (all (>1))
        even <- check "that gaps between chosen elements are almost equal" $ 
                   (gaps $ getKElems 3 [0..19]) `shouldSatisfy` withinOne
                <> (gaps $ getKElems 5 [0..9]) `shouldSatisfy` withinOne
        most <- check "that most of the range is used" $ getKElems 4 [0..13] `containsOne` [12,13]
        range <- check "that the full range is used" $  
                   getKElems 3 [1..13] `containsAll` [1,13] 
                <> getKElems 2 "Hello" `containsAll` "Ho"
        middle <- check "that (one of) the middle element(s) is included" $ 
                   getKElems 3 [1..9] `shouldContain` 5
                <> getKElems 3 [1..10] `containsOne` [5,6] 
        five <- check "that getKElems 5 [0..18] distributes evenly" $ 
              (gaps $ getKElems 5 [0..18]) `shouldSatisfy`  fiveSpacing
        nine <- check "that getKElems 9 [0..19] distributes evenly" $ 
              (getKElems 9 [0..19]) `shouldSatisfy` (gapSpacing 2.375)
        eight <- check "that getKElems 8 [0..19] distributes evenly" $ 
              (getKElems 8 [0..19]) `shouldSatisfy` (gapSpacing (3-2/7))
        return $ do
          whenM (passedAll [spread, even, most]) $ passWith 1
          whenM (passedAll [spread, even]) $ addPoints 2
          whenM (passedAll [range, middle]) $ addPoints 2
          whenM allPassed $ addPoints 2
          
        where gaps :: [Integer] -> [Integer]
              gaps lst = zipWith (-) (tail lst) lst
              withinOne lst = ((maximum lst) - (minimum lst)) <=1
              fiveSpacing [a,b,c,d] = a+b == c+d && (a+b+c+d) == 18
              fiveSpacing _ = error "Didn't return five elements!"
              gapSpacing gap lst = all (`elem` [0,1]) $ zipWith (\i e -> e - truncate i) [0,gap..] lst


implementation :: Grader String
implementation = assess "full credit: implementation details" 11 $ do
        min <- check "that assignPoint uses minimize" $ "assignPoint" `shouldCall` "minimize"
        buck <- check "that assignPoints uses bucket" $ "assignPoints" `shouldCall` "bucket"
        maybeJ <- check "that moveCenters eschews fromJust" $ noneShouldCall "fromJust" 
        maybeN <- check "that moveCenters doesn't crash on Nothings" $ moveCenters eucDist [((0, 0, 0), [])] `shouldBe` []
        schlem <- check "that kMeans is not schlemiely" $ shouldNotRepeatWork "kMeans"
        return $ useWeights [(min, 3), (buck, 3), (maybeJ, 2), (maybeN, 1), (schlem, 2)]

styleTests :: Grader String
styleTests = assess "(mostly) optional style tests" 0 $ do
        check "for unnecessary pattern matching" $ noneShouldHaveUnusedPatterns
        check "for repeated work" $ noneShouldRepeatWork
        check "that you don't concatenate a single element" $ noneShouldHaveBadConcats
        check "that you don't use length instead of null" $ noneShouldHaveBadLength
        check "that you eschew head or tail (can be unsafe, but not always bad)" $ noneShouldCall "head" <> noneShouldCall "tail"
        check "that you pattern match over fst/snd" $ noneCouldMatchTuple
        check "for nested list comprehensions" $ noneShouldUseNestedLC
        check "for ifs that could be boolean expressions" $ noneShouldHaveBadIfs
        check "for unneeded parens" $ noneShouldHaveUnneededParens
        noIndex <- check "don't use (!!)" $ noneShouldCall "!!"
        return $ ifM (passed noIndex) (passWith 0) (failWith (-2))
                  
tree :: Grader String
tree = describe "Project 3" $ do
        describe "milestone" $ do
                testGetKElems
                testEucDist
                testChebyDist
                testTownshipDist
                testTrafficDist
                testAverage
                testBucket
                testMinimize
        checkpoint
        describe "core project" $ do
                testAssignPoint
                testAssignPoints
                testFindMean
                testMoveCenters
                testImproveClusters
                testKMeans
        checkpoint
        describe "full credit tests" $ do
                testReplaceMissingClusters
                implementation
                testGetKElemsFC
        styleTests

runTests :: Int -> Bool -> IO ()
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()
