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

{- 
Extracts a list of unique cities from a given roadmap.
The function takes the roadmap, which is a list of tuples representing roads between cities, and returns a list of unique 'City' values.
Arguments: 
    Roadmap -> Defined as a list of tuples where each tuple contains two cities the distance between them.
-}
cities :: RoadMap -> [City]
cities road = eliminateDuplicates (citiesAux road)

{-
Helper function that accumulates all cities from a given roadmap.
This recusive function collects all the cities resent in the roadmap, returning a list of cities.
Arguments: 
    Roadmap -> Defined as a list of tuples where each tuple contains two cities the distance between them.
-}
citiesAux :: RoadMap -> [City]
citiesAux [] = []
citiesAux ((a,b,_):others) = [a,b] ++ cities (others)

{- 
Eliminates duplicate cities from a list.
The function takes a list of cities and returns a new list containing only unique cities. This is achieved by recursively filtering out duplicates.
Arguments:
    [City] -> A list of 'City' values that may contain duplicates.
-}
eliminateDuplicates :: [City] -> [City]
eliminateDuplicates [] = []
eliminateDuplicates (x:xs) = x : eliminateDuplicates (filter (/= x) xs)

---

{- 
Returns a boolean indicating whether two cities are linked directly.
This function checks the roadmap to determine if there is a direct connection between the two specified cities.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The first city to check for adjacency.
    City -> The second city to check for adjacency.
-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent road city1 city2 = areAdjacentAux road city1 city2 || areAdjacentAux road city2 city1

{- 
Helper function to check if two cities are directly linked.
This recursive function traverses the roadmap to find a direct road between the two specified cities.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The first city to check for adjacency.
    City -> The second city to check for adjacency.
-}
areAdjacentAux :: RoadMap -> City -> City -> Bool
areAdjacentAux [] _ _ = False
areAdjacentAux ((a,b,_):others) city1 city2 = 
    if (a == city1 && b == city2) then True
    else areAdjacentAux others city1 city2

---

{- 
Returns a Maybe value indicating the distance between two directly connected cities.
This function takes two city names and looks for their direct connection in the roadmap, returning their distance if found.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The first city to check for distance.
    City -> The second city to check for distance.
-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance road city1 city2 =
    case (distanceAux road city1 city2, distanceAux road city2 city1) of
        (Just distance, Nothing) -> Just distance
        (Nothing, Just distance) -> Just distance
        (Nothing, Nothing) -> Nothing

{- 
Helper function to find the distance between two directly connected cities.
This recursive function searches through the roadmap to retrieve the distance if a direct connection exists.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The first city to check for distance.
    City -> The second city to check for distance.
-}
distanceAux :: RoadMap -> City -> City -> Maybe Distance
distanceAux [] _ _ = Nothing
distanceAux ((a,b,distance):others) city1 city2 = 
    if (a == city1 && b == city2) then Just distance
    else distanceAux others city1 city2

---

{- 
Returns the cities adjacent to a particular city and the respective distances to them.
This function retrieves a list of tuples containing cities that are directly connected to the specified city, along with the distances to those cities.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the city for which adjacent cities are to be found.
-}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent road city = adjacentAuxA road city ++ adjacentAuxB road city

{- 
Helper function to find all cities directly connected to the specified city, where the specified city is the first city in the connection.
This function recursively checks each road in the roadmap to find adjacent cities and their distances.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the city for which adjacent cities are to be found.
-}
adjacentAuxA :: RoadMap -> City -> [(City, Distance)]
adjacentAuxA [] _ = []
adjacentAuxA ((a, b, d):others) city =
    if (a == city) then [(b, d)] ++ adjacentAuxA others city
    else adjacentAuxA others city

{- 
Helper function to find all cities directly connected to the specified city, where the specified city is the second city in the connection.
This function recursively checks each road in the roadmap to find adjacent cities and their distances.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the city for which adjacent cities are to be found.
-}
adjacentAuxB :: RoadMap -> City -> [(City, Distance)]
adjacentAuxB [] _ = []
adjacentAuxB ((a, b , d):others) city = 
    if (b == city) then [(a, d)] ++ adjacentAuxB others city
    else adjacentAuxB others city

---

{- 
Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing.
This function calculates the total distance of a specified path by summing the distances of each directly connected pair of cities in the path.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    Path -> a list of cities representing the route taken from the start city to the end city.
-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance road (p1:p2:ps) = calculateTotalDistance road ((p1, p2) : zip (p2:ps) ps)

{- 
Helper function to calculate the total distance of a list of city pairs in a path.
This function recursively sums the distances between each pair of cities in the list, returning the total distance wrapped in a Just value.
If any pair of cities is not directly connected, it returns Nothing.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    [(City, City)] -> a list of tuples containing pairs of cities for which the distance needs to be calculated.
-}
calculateTotalDistance :: RoadMap -> [(City, City)] -> Maybe Distance
calculateTotalDistance _ [] = Just 0
calculateTotalDistance road ((a, b):xs) = do
  dist <- distance road a b
  restDist <- calculateTotalDistance road xs
  return (dist + restDist)

---

{- 
Returns the names of the cities with the highest number of roads connecting to them (i.e., the vertices with the highest degree).
This function counts the number of connections (roads) for each city and returns a list of cities with the maximum connection count.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
-}
rome :: RoadMap -> [City]
rome roadMap =
  let
    connectionsCount = romeAux roadMap
    maxDegree = maximum (map snd connectionsCount)
  in [city | (city, degree) <- connectionsCount, degree == maxDegree]

{- 
Helper function to count the number of connections for each city in the roadmap.
This function creates a list of tuples where each tuple contains a city and its corresponding connection count.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
-}
romeAux :: RoadMap -> [(City, Int)]
romeAux roadMap =
  let
    allCities = [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]
    groupedCities = Data.List.group $ Data.List.sort allCities
    in [(c, length cities) | (c: cities) <- groupedCities]

---

{- 
Returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city).
This function performs a breadth-first search (BFS) to check if all cities can be reached from a starting city.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = 
    let 
        allCities = cities roadMap
        startCity = head allCities
        visitedCities = bfs roadMap startCity
    in 
        length (eliminateDuplicates visitedCities) == length allCities

{- 
Performs a breadth-first search (BFS) to find all cities reachable from the starting city.
This function recursively explores the roadmap to gather all cities that can be reached from the specified starting city.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the starting city for the BFS traversal.
-}
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

{- 
Computes all shortest paths connecting the two cities given as input.
This function utilizes breadth-first search to explore all possible paths between the start and end cities, and it filters out the valid paths to find those with the minimum distance.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the starting city for the path search.
    City -> the ending city for the path search.
-}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end =
    let allPaths = bfsPaths roadMap start end  -- Get all paths using BFS
        -- Calculate distances for each path, using Just or Nothing
        distances = [(path, pathDistance roadMap path) | path <- allPaths]
        -- Filter out valid paths and find the minimum distance
        validDistances = [dist | (path, Just dist) <- distances]
        minDistance = minimum validDistances  -- Find the minimum distance
    in [path | (path, Just dist) <- distances, dist == minDistance]  -- Filter paths by the minimum distance

{- 
Helper function to find all paths between two cities using breadth-first search (BFS).
This function recursively explores the roadmap to find all unique paths from the starting city to the ending city.
Arguments:
    RoadMap -> defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> the starting city for the path search.
    City -> the ending city for the path search.
-}
bfsPaths :: RoadMap -> City -> City -> [Path]
bfsPaths roadMap start end = bfsPaths' [(start, [start])] end [] []  
    where
        bfsPaths' :: [(City, Path)] -> City -> [City] -> [Path] -> [Path]
        bfsPaths' [] _ _ paths = paths
        bfsPaths' ((currentCity,path):queue) end visited paths
            | currentCity == end = bfsPaths' queue end visited (paths ++ [path])
            | currentCity `elem` visited = bfsPaths' queue end visited paths
            | otherwise =
                let 
                    adj = [nextCity | (nextCity, _) <- adjacent roadMap currentCity]
                    newPaths = [(nextCity, path ++ [nextCity]) | nextCity <- adj, nextCity `notElem` visited]
                in 
                    bfsPaths' (queue ++ newPaths) end (currentCity : visited) paths


---

-- travelSales :: RoadMap -> Path, given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the start-
-- ing town. The problem is to find the shortest route, that is, the route whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be ac-
-- cepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. Note that the roadmap might not be a complete graph (i.e. a graph where all
-- vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.

{- 
Returns a Path that represents a solution to the Traveling Salesman Problem (TSP) for the given roadmap.
This function seeks to find the shortest route that visits each city exactly once and returns to the starting city.
If no valid TSP path exists, an empty list is returned.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
-}
travelSales :: RoadMap -> Path
travelSales roadMap 
    | not (isStronglyConnected roadMap) = []
    | otherwise =
        let
            startCity = head (cities roadMap)
        in
            case bfsTravel [[startCity]] of
                [] -> []
                (x:xs) -> x
    where
        {- 
        Breadth-first search to explore all potential paths from a list of paths.
        This function extends existing paths by adding neighbors and filters complete paths.
        Arguments:
            paths -> A list of currently explored paths to be extended.
        Returns:
            A list of complete paths that have been found.
        -}
        bfsTravel :: [Path] -> [Path]
        bfsTravel [] = []
        bfsTravel paths =
            let allPaths = concatMap extendPath paths
                complete = filter isComplete allPaths
                incomplete = filter (not . isComplete) allPaths
            in if null incomplete
               then shortestPathAdap roadMap complete
               else (complete ++ bfsTravel incomplete)

        {- 
        Checks if a given path is complete.
        A path is considered complete if it visits all cities exactly once and returns to the starting city.
        Arguments:
            path -> A list representing the current path.
        Returns:
            True if the path is complete, False otherwise.
        -}
        isComplete :: Path -> Bool
        isComplete (x:xs) = 
            let
                allCities = cities roadMap
            in length (x:xs) == (length allCities + 1) && (x == last xs)

        {- 
        Extends a given path by adding neighboring cities.
        This function generates new paths by appending unvisited neighbors of the current city.
        Arguments:
            Path -> A list representing the current path being extended.
        Returns:
            A list of new paths created by adding neighbors to the current path.
        -}
        extendPath :: Path -> [Path]
        extendPath (x:xs) = 
            let 
                currentCity = last (x:xs)
                neighbors = cityNeighbors roadMap currentCity
                allCities = cities roadMap
            in [(x:xs) ++ [city] | city <- neighbors, city `notElem` (x:xs) || (city == x && length (x:xs) == length allCities) ]


{- 
Returns a list of cities that are directly connected to a specified city.
This function collects neighboring cities from the roadmap, accounting for connections where the specified city appears in either position.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The city for which neighbors are to be found.
-}
cityNeighbors :: RoadMap -> City -> [City]
cityNeighbors road city = cityNeighborsAuxA road city ++ cityNeighborsAuxB road city

{- 
Helper function to find neighboring cities when the specified city matches the first element of the tuple.
This function traverses the roadmap to gather all cities directly connected to the specified city.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The city for which neighbors are being searched.
-}
cityNeighborsAuxA :: RoadMap -> City -> [City]
cityNeighborsAuxA [] _ = []
cityNeighborsAuxA ((a, b, d):others) city =
    if (a == city) then [b] ++ cityNeighborsAuxA others city
    else cityNeighborsAuxA others city

{- 
Helper function to find neighboring cities when the specified city matches the second element of the tuple.
This function traverses the roadmap to gather all cities directly connected to the specified city.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    City -> The city for which neighbors are being searched.
-}
cityNeighborsAuxB :: RoadMap -> City -> [City]
cityNeighborsAuxB [] _ = []
cityNeighborsAuxB ((a, b, d):others) city = 
    if (b == city) then [a] ++ cityNeighborsAuxB others city
    else cityNeighborsAuxB others city

{- 
Filters the given paths to return those with the shortest distance.
This function computes the distances of all provided paths and filters them to include only the shortest ones.
Arguments:
    RoadMap -> Defined as a list of tuples where each tuple contains two cities and the distance between them.
    paths -> A list of paths to be evaluated for distance.
Returns:
    A list of paths that have the minimum distance among the evaluated paths.
-}
shortestPathAdap :: RoadMap -> [Path] -> [Path]
shortestPathAdap roadMap paths =
    let 
        distances = [(path, pathDistance roadMap path) | path <- paths]
        -- Filter out valid paths and find the minimum distance
        validDistances = [dist | (path, Just dist) <- distances]
        minDistance = minimum validDistances  -- Find the minimum distance
    in [path | (path, Just dist) <- distances, dist == minDistance]  -- Filter paths by the minimum distance



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