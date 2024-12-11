# PFLT11_G08
Yet Another Country-Graph Program

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


- **Project name:** PFLT11_G08 - Yet Another Country-Graph Program
- **Short description:** Graph-based Interactive Console Program with Cities in a Country
- **Environment:** Unix Console
- **Tools:** Haskell
- **Institution:** [FEUP](https://sigarra.up.pt/feup/en/web_page.Inicial)
- **Course:** [PFL](https://sigarra.up.pt/feup/en/ucurr_geral.ficha_uc_view?pv_ocorrencia_id=541889) (Functional and Logic Programming)
- **Project grade:** TBD
- **Group members:**
    - Leonor Couto (up202205796@fe.up.pt) 50%
    - Rodrigo de Sousa (up202205751@fe.up.pt) 50%

---

## ShortestPath

## TravelSalesman

## PFL - Haskell Coursework
### Licenciatura em Engenharia Informática e Computação
### October 2024

- This assignment counts for 25% of the final grade. The evaluation focuses on
implemented features, the quality and efficiency of the code and respective com-
ments, the readme file, and participation in the assignment and presentation.
Grades may differ between group members.


- The work must be submitted by 20:00 of the 3rd of November, 2024, in
the activity to be made available for this purpose on Moodle, with demonstra-
tions carried out during the week of November 4th, 2024. The demonstrations
should be scheduled with the teacher of each practical class.


3.1 Source code file
Assignments will be subjected to an automated battery of tests, so you
must adhere to the stated file and function naming conventions. The
inability to test the developed code will result in penalties in the evaluation.
The following rules must be followed carefully:
• The implemented assignment must run under GHC, version 9.10.1.
• All the work must be developed in a single file named TP1.hs. If you
want to separate your code, do so with comments.
• The work must be developed using the starter TP1.hs file provided in
the course’s Moodle page. The datatype definitions and function
signatures of this file must not be modified.
• All of the functions are initially defined as undefined (which is a reseved
keyword in Haskell). If you do not implement a version of a function asked
in the assignment, leave it as is (i.e. as undefined).
• All code must be properly commented: each function should include
the type declaration (signature) as well a brief description of the func-
tion’s goal and the meaning of the arguments.
• You may import the following three GHC modules: Data.List, Data.Array
and Data.Bits. You may import all, some or none of them. The import
statements must be the first lines of the code (i.e. do not include lines with
comments before the import statements). Each import statement, which
must be in exactly a single line of code, must have exactly the following
format: import qualified <name of the module>. For instance, if a
group wants to use all three modules, the first 3 lines of their source code
file are exactly (in no particular order):
import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


3.2 README file
The README file should contain the following sections:
• Identification of the group members, contribution of each member (in
percentages adding up to 100%) and a brief description the tasks each one
performed.
• Explanation of how the shortestPath function was implemented, includ-
ing a justification of why certain auxiliary data structures were selected
and used, and a description of the algorithm(s) used.
• A similar section to the previous one for the travelSales function.



4 Assignment description
The goals of this project are to define and use appropriate data types for a
graph representing a country, composed of a set of interconnected cities.
4.1 Types
The type for graphs used as input of all the functions to be implemented is:
type RoadMap = [(City,City,Distance)]
which must use the following auxiliary types:
type City = String
type Distance = Int
type Path = [City]
According to these definitions, the vertices of the graph are the cities and each
edge is a tuple containing the name of the cities it connects and the distance of
the road connecting them. Assume that all distances are represented as integer
numbers. Consider that the graph is undirected.

4.2 Functions
Implement the following operations needed to manipulate roadmaps. Unless we
explicitly ask for a particular order, in functions where the output is a list, any
order of its elements will be accepted.
1. cities :: RoadMap -> [City], returns all the cities in the graph.
2. areAdjacent :: RoadMap -> City -> City -> Bool, returns a boolean
indicating whether two cities are linked directly.
3. distance :: RoadMap -> City -> City -> Maybe Distance, returns a
Just value with the distance between two cities connected directly, given
two city names, and Nothing otherwise.
4. adjacent :: RoadMap -> City -> [(City,Distance)], returns the cities
adjacent to a particular city (i.e. cities with a direct edge between them)
and the respective distances to them.
5. pathDistance :: RoadMap -> Path -> Maybe Distance, returns the sum
of all individual distances in a path between two cities in a Just value, if all
the consecutive pairs of cities are directly connected by roads. Otherwise,
it returns a Nothing.
6. rome :: RoadMap -> [City], returns the names of the cities with the
highest number of roads connecting to them (i.e. the vertices with the
highest degree).
7. isStronglyConnected :: RoadMap -> Bool, returns a boolean indicat-
ing whether all the cities in the graph are connected in the roadmap (i.e.,
if every city is reachable from every other city).
8. shortestPath :: RoadMap -> City -> City -> [Path], computes all
shortest paths [RL99, BG20] connecting the two cities given as input.
Note that there may be more than one path with the same total distance.
If there are no paths between the input cities, then return an empty list.
Note that the (only) shortest path between a city c and itself is [c].
9. travelSales :: RoadMap -> Path, given a roadmap, returns a solution
of the Traveling Salesman Problem (TSP). In this problem, a traveling
salesperson has to visit each city exactly once and come back to the start-
ing town. The problem is to find the shortest route, that is, the route
whose total distance is minimum. This problem has a known solution
using dynamic programming [RL99]. Any optimal TSP path will be ac-
cepted and the function only needs to return one of them, so the starting
city (which is also the ending city) is left to be chosen by each group. Note
that the roadmap might not be a complete graph (i.e. a graph where all
vertices are connected to all other vertices). If the graph does not have a
TSP path, then return an empty list.


The time complexity of the algorithms will be taken into account in the
coursework grades. Therefore, to achieve more efficient solutions, especially
in the last two functions, students are advised to use the data structures for
the GHC modules that can be imported and to convert the graph to a more
convenient representation. Some example of graph representations include:

1. Adjacency list representation
type AdjList = [(City,[(City,Distance)]]
2. Adjacency matrix representation type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)1
3. Pointer representation
data AdjPointers = Place City [(AdjPointers, Distance)]

The pointer representation is very space efficient but its operations are a bit
more complicated and time inefficient.
When comparing between the adjacency list and the adjacency matrix repre-
sentations, the efficiencies of their manipulating functions depend on the number
of cities and roads. Informally, when there is a large number of roads, the map
is said to be dense, and when there are few roads, it is sparse. In general, the
matrix representation is better with dense maps and the adjacency list repre-
sentation with sparse maps.



References
[BG20] Richard Bird and Jeremy Gibbons. Algorithm Design with Haskell.
Cambridge University Press, 2020.
[RL99] Fethi Rabhi and Guy Lapalme. Algorithms: a functional programming
approach. Addison-Wesley, 2 edition, 1999.
