import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

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

-- 1. cities returns all the cities in the graph
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

-- 3. distance returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise
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

-- 4. adjacent returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them
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

-- 5. pathDistance returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance road (p1:p2:ps) = calculateTotalDistance road ((p1, p2) : zip (p2:ps) ps)

calculateTotalDistance :: RoadMap -> [(City, City)] -> Maybe Distance
calculateTotalDistance _ [] = Just 0
calculateTotalDistance road ((a, b):xs) = do
  dist <- distance road a b
  restDist <- calculateTotalDistance road xs
  return (dist + restDist)

---

-- rome returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)
rome :: RoadMap -> [City]
rome roadMap =
  let
    connectionsCount = romeAux roadMap
    maxDegree = maximum (map snd connectionsCount)
  in [city | (city, degree) <- connectionsCount, degree == maxDegree]


romeAux :: RoadMap -> [(City, Int)]
romeAux roadMap =
  let
    allCities = [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]
    groupedCities = Data.List.group $ Data.List.sort allCities
    in [(c, length cities) | (c: cities) <- groupedCities]

---

-- isStronglyConnected :: RoadMap -> Bool, returns a boolean indicat-ing whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city)
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = 
    let 
        allCities = cities roadMap
        startCity = head allCities
        visitedCities = bfs roadMap startCity
    in 
        length (eliminateDuplicates visitedCities) == length allCities


bfs :: RoadMap -> City -> [City]
bfs roadMap start = bfs' [start] []
    where
        bfs' :: [City] -> [City] -> [City]
        bfs' [] visited = visited
        bfs' (currentCity:queue) visited
            | currentCity `elem` visited = bfs' queue visited
            | otherwise =
                let 
                    adj = [nextCity | (nextCity, _) <- adjacent roadMap currentCity]
                in 
                    bfs' (queue ++ adj) (currentCity : visited)
            


---

-- shortestPath computes all shortest paths [RL99, BG20] connecting the two cities given as input.
-- Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c].
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

---

-- travelSales :: RoadMap -> Path, given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the start-
-- ing town. The problem is to find the shortest route, that is, the route whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be ac-
-- cepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. Note that the roadmap might not be a complete graph (i.e. a graph where all
-- vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.
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

-- Fully connected small graph
gTest4 :: RoadMap
gTest4 = [("A", "B", 5), ("A", "C", 10), ("B", "C", 3), ("B", "D", 8), ("C", "D", 2)]

-- Disconnected graph with two separate components
gTest5 :: RoadMap
gTest5 = [("A", "B", 7), ("B", "C", 4), ("D", "E", 6)]

-- Circular route where each city is connected in a loop
gTest6 :: RoadMap
gTest6 = [("A", "B", 1), ("B", "C", 1), ("C", "D", 1), ("D", "A", 1)]

-- Single city with no roads
gTest7 :: RoadMap
gTest7 = [("A", "A", 0)]  -- Or simply [], representing a single city with no connections

-- Tree structure (connected but with no cycles)
gTest8 :: RoadMap
gTest8 = [("A", "B", 3), ("A", "C", 4), ("B", "D", 2), ("B", "E", 1), ("C", "F", 6), ("C", "G", 5)]

-- Star structure (one central city connected to all others)
gTest9 :: RoadMap
gTest9 = [("Center", "A", 2), ("Center", "B", 4), ("Center", "C", 6), ("Center", "D", 8)]




-- Menu

main :: IO ()
main = do
    putStrLn "\nSelect an option:"
    putStrLn "1. List all cities"
    putStrLn "2. Check if two cities are adjacent"
    putStrLn "3. Get distance between two cities"
    putStrLn "4. Find adjacent cities and distances"
    putStrLn "5. Calculate path distance"
    putStrLn "6. Find cities with the highest number of connections"
    putStrLn "7. Check if graph is strongly connected"
    putStrLn "8. Find shortest path between two cities"
    putStrLn "9. Solve traveling salesman problem (TSP)"
    putStrLn "0. Exit"
    putStrLn "Enter your choice (0-9): "
    choice <- getLine

    if choice == "0"
        then putStrLn "Exiting program."
        else do
            putStrLn "\nChoose a RoadMap:"
            putStrLn "1. gTest1"
            putStrLn "2. gTest2"
            putStrLn "3. gTest3"
            putStrLn "4. gTest4"
            putStrLn "5. gTest5"
            putStrLn "6. gTest6"
            putStrLn "7. gTest7"
            putStrLn "8. gTest8"
            putStrLn "9. gTest9"
            putStrLn "0. Custom input"
            putStrLn "Enter your choice (1-4): "
            mapChoice <- getLine
            roadMap <- case mapChoice of
                "1" -> return gTest1
                "2" -> return gTest2
                "3" -> return gTest3
                "4" -> return gTest4
                "5" -> return gTest5
                "6" -> return gTest6
                "7" -> return gTest7
                "8" -> return gTest8
                "9" -> return gTest9
                "0" -> getCustomRoadMap
                _   -> do
                    putStrLn "Invalid choice, defaulting to gTest1."
                    return gTest1

            runFunction choice roadMap
            main

---

runFunction :: String -> RoadMap -> IO ()
runFunction choice roadMap = case choice of
    "1" -> print (cities roadMap)
    "2" -> do
        (city1, city2) <- getCityPair
        print (areAdjacent roadMap city1 city2)
    "3" -> do
        (city1, city2) <- getCityPair
        print (distance roadMap city1 city2)
    "4" -> do
        city <- getCity
        print (adjacent roadMap city)
    "5" -> do
        path <- getPath
        print (pathDistance roadMap path)
    "6" -> print (rome roadMap)
    "7" -> print (isStronglyConnected roadMap)
    "8" -> do
        (start, end) <- getCityPair
        print (shortestPath roadMap start end)
    "9" -> print (travelSales roadMap)
    _   -> putStrLn "Invalid option selected."

---

getCity :: IO City
getCity = do
    putStrLn "Enter city name: "
    getLine

getCityPair :: IO (City, City)
getCityPair = do
    putStrLn "Enter the first city: "
    city1 <- getLine
    putStrLn "Enter the second city: "
    city2 <- getLine
    return (city1, city2)

getPath :: IO Path
getPath = do
    putStrLn "Enter a path (list of city names separated by spaces): "
    fmap words getLine

getCustomRoadMap :: IO RoadMap
getCustomRoadMap = do
    putStrLn "Enter roads in the format (city1, city2, distance), one per line. Enter an empty line to finish."
    getRoadEntries []

getRoadEntries :: RoadMap -> IO RoadMap
getRoadEntries entries = do
    line <- getLine
    if null line
        then return entries
        else case readMaybe line of
            Just road -> getRoadEntries (road : entries)
            Nothing   -> do
                putStrLn "Invalid format. Enter again (e.g., (\"City1\", \"City2\", 10))."
                getRoadEntries entries

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(val, "")] -> Just val
    _           -> Nothing