--import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

-- commenting info (delete at the end): each function should include
-- the type declaration (signature) as well a brief description of the function’s goal
-- and the meaning of the arguments.

---

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

---

-- 1. cities returns all the cities in the graph.
cities :: RoadMap -> [City]
cities road = eliminateDuplicates (citiesAux road)

citiesAux :: RoadMap -> [City]
citiesAux [] = []
citiesAux ((a,b,_):others) = [a,b] ++ cities (others)

eliminateDuplicates :: [City] -> [City]
eliminateDuplicates [] = []
eliminateDuplicates (x:xs) = x : eliminateDuplicates (filter (/= x) xs)


---

-- 2. areAdjacent returns a boolean indicating whether two cities are linked directly
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent road city1 city2 = areAdjacentAux road city1 city2 || areAdjacentAux road city2 city1

areAdjacentAux :: RoadMap -> City -> City -> Bool
areAdjacentAux [] _ _ = False
areAdjacentAux ((a,b,_):others) city1 city2 = 
    if (a == city1 && b == city2) then True
    else areAdjacentAux others city1 city2

---

-- 3. distance returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.
distance :: RoadMap -> City -> City -> Maybe Distance
distance road city1 city2 =
    case (distanceAux road city1 city2, distanceAux road city2 city1) of
        (Just dance, Nothing) -> Just dance
        (Nothing, Just dance) -> Just dance
        (Nothing, Nothing) -> Nothing

distanceAux :: RoadMap -> City -> City -> Maybe Distance
distanceAux [] _ _ = Nothing
distanceAux ((a,b,dance):others) city1 city2 = 
    if (a == city1 && b == city2) then Just dance
    else distanceAux others city1 city2

---

-- 4. adjacent returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them.
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent road city = adjacentAuxA road city ++ adjacentAuxB road city

adjacentAuxA :: RoadMap -> City -> [(City, Distance)]
adjacentAuxA [] _ = []
adjacentAuxA ((a, b, d):others) city =
    if (a == city) then [(b, d)] ++ adjacentAuxA others city
    else adjacentAuxA others city

adjacentAuxB :: RoadMap -> City -> [(City, Distance)]
adjacentAuxB [] _ = []
adjacentAuxB ((a, b , d):others) city = 
    if (b == city) then [(a, d)] ++ adjacentAuxB others city
    else adjacentAuxB others city

---

-- 5. pathDistance returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance road path = calculateTotalDistance road (zip path (tail path))

calculateTotalDistance :: RoadMap -> Path -> Maybe Distance
calculateTotalDistance _ [] = Nothing
calculateTotalDistance _ [_] = Nothing
calculateTotalDistance road (a:b:xs) = do
  dist <- distance road a b
  restDist <- calculateTotalDistance road (b:xs)
  return (dist + restDist)

---

rome :: RoadMap -> [City]
rome = undefined

---

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

---

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

---

travelSales :: RoadMap -> Path
travelSales = undefined

---

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

---

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]