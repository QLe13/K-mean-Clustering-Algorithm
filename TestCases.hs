module TestCases where
import Cluster


-- getKElems tests
lst1 = "AZ#*"
lst2 = [1..100]

-- eucDist tests, others can be infered.
pointA = (0,0,10) 
pointB = (1,1,10) 
pointC = (0,0,10)
pointD = (0,2,10)
pointE = (0,2,20)
pointF = (0,5.5,10)

--minimze tests
--using (fromIntegral.length)
findSmallest =  ["aaaa", "b"]
findSmaller = ["a","c","aa"]

--bucket tests
--using length
bucketsA = [1..3]
elemsA = ["Hi","my","job","is","fun","!"]
bucketsB = []
elemsB = []

--assignPoints tests
                
--with eucDist
centersA=[(0,0,-1),(5,7,-1)] 
pntA =(5,0,100)
--answer: (0.0,0.0,-1)

--with trafficDist
centersB=[(0,0,-1),(5,7,-1)] 
pntB=(5,0,100) 
--answer: (5.0,7.0,-1)

--with manhatDist
centersC=[(0,0,100),(2,0,200)]
pntC=(5,0,100) 
--answer: (2.0,0.0,200)

--with townshipDist
centersD=[(0,0,100),(2,0,200)]
pntD=(5,0,100) 
--answer: (0.0,0.0,100)

--with eucDist
centersE=[(0,0,3), (10,10,4), (2,2,1)]
pntE=(4,4,-1) 
--answer: (2,2,1)

--cases for findMean and townshipDistance
--i.e. try findMean townshipDist fmNaiveCase
--All should be (4,4,1), except fmOneCase whcih shoudl be 4,4,2
fmNaiveCase,fmZeroCase,fmOneCase,fmNegCase :: Cluster
fmNaiveCase= ((3,3,8), [(1,1,8), (9,9,8), (2,2,1)])
fmZeroCase = ((3,3,0), [(1,1,0), (9,9,0), (2,2,1)])
fmOneCase  = ((3,3,1), [(1,1,1), (9,9,1), (2,2,2)])
fmNegCase  = ((3,3,-1), [(1,1,-1), (9,9,-1), (2,2,1)])

--cases for moveCenters
moveCenterCase1 = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

moveCenterCase2 = [(1.0666666666666667,1.0666666666666667,300),(6.742857142857143,7.071428571428571,900)]

--cases for assignPoints, improveClusters, and kMeans
tenPoints, closePoints, closeCenters, closeCentersEmpty :: [(Double,Double,Int)]
tenPoints = [(7,1,100), (7,3,200), (8,1,300), (8,2,400), (7.5,3.5,500), (2,7,600), (1,7,700), (3,7,800), (2,8,900), (2,6,1000)]

closePoints = [(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400),(7.0,7.1,200),(7.5,7.5,500),
               (6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)]
closeCenters = [(1.0,1.0,100), (7,7,200)]
closeCentersEmpty = [(1.0,1.0,100),(0,0,300),(7,7,200)]
testAnswer,closeAnswer,closeAnswerEmpty :: [((Double,Double,Int),[(Double,Double,Int)])]
testAnswer =  [((1.0,1.0,-1),[(1.0,7.0,700)]),
                     ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),
                                    (7.5,3.5,500),(2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]
closeAnswer = [((1.0,1.0,100),[(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400)]),
                        ((7.0,7.0,200),[(7.0,7.1,200),(7.5,7.5,500),(6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)])]
closeAnswerEmpty = ((0.0,0.0,300),[]):closeAnswer
