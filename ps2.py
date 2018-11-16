# 6.0002 Problem Set 5
# Graph optimization
# Name:
# Collaborators:
# Time:

#
# Finding shortest paths through MIT buildings
#
import unittest
from graph import Digraph, Node, WeightedEdge
import os
import pdb
#
# Problem 2: Building up the Campus Map
#
# Problem 2a: Designing your graph
#
# What do the graph's nodes represent in this problem? What
# do the graph's edges represent? Where are the distances
# represented?
#
# Answer:
#


# Problem 2b: Implementing load_map
def load_map(map_filename):
    """
    Parses the map file and constructs a directed graph

    Parameters:
        map_filename : name of the map file

    Assumes:
        Each entry in the map file consists of the following four positive
        integers, separated by a blank space:
            From To TotalDistance DistanceOutdoors
        e.g.
            32 76 54 23
        This entry would become an edge from 32 to 76.

    Returns:
        a Digraph representing the map
    """
    g = Digraph()
    with open(map_filename) as f:
        data = f.read().strip()
    dataList = data.split('\n')
    for mapEdge in dataList:
        edgeList = mapEdge.split(' ') # from to TD, DO
        fromN = Node(edgeList[0])
        toN = Node(edgeList[1])
        if not g.has_node(fromN):
            g.add_node(fromN)
        if not g.has_node(toN):
            g.add_node(toN)   
        g.add_edge(WeightedEdge(fromN,toN,edgeList[2],edgeList[3]))
    return g

# Problem 2c: Testing load_map
# Include the lines used to test load_map below, but comment them out
#
# Problem 3: Finding the Shorest Path using Optimized Search Method
#
# Problem 3a: Objective function
#
# What is the objective function for this problem? What are the constraints?
#
# Answer:
#
def get_total_distance(g, path):
    td=0
    for i in range(len(path)-1):
        node = path[i]
        for edge in g.edges[node]:
            if edge.get_destination() == path[i+1]:
                td += int(edge.get_total_distance())
    return td
    
def get_outdoor_distance(g, path):
    td=0
    for i in range(len(path)-1):
        node = path[i]
        for edge in g.edges[node]:
            if edge.get_destination() == path[i+1]:
                td += int(edge.get_outdoor_distance())
    return td

# Problem 3b: Implement get_best_path
def get_best_path(g, start, end, path, max_dist_outdoors, best_dist,
                  best_path, w):
    """
    Finds the shortest path between buildings subject to constraints.

    Parameters:
        digraph: Digraph instance
            The graph on which to carry out the search
        start: string
            Building number at which to start
        end: string
            Building number at which to end
        path: list composed of [[list of strings], int, int]
            Represents the current path of nodes being traversed. Contains
            a list of node names, total distance traveled, and total
            distance outdoors.
        max_dist_outdoors: int
            Maximum distance spent outdoors on a path
        best_dist: int
            The smallest distance between the original start and end node
            for the initial problem that you are trying to solve
        best_path: list of strings
            The shortest path found so far between the original start
            and end node.

    Returns:
        A tuple with the shortest-path from start to end, represented by
        a list of building numbers (in strings), [n_1, n_2, ..., n_k],
        where there exists an edge from n_i to n_(i+1) in digraph,
        for all 1 <= i < k and the distance of that path.

        If there exists no path that satisfies max_total_dist and
        max_dist_outdoors constraints, then return None.
    """
    w_key = str(start)+':'+str(path[2])
    if not g.has_node(start) or not g.has_node(end):
        raise ValueError('Those nodes don\'t exist fool')
    elif path[0] != None and path[2] > max_dist_outdoors: #too much time outdoors!
        return ([None , None, None], w)
    elif best_dist != None and path[1] >= best_dist:
        return ([None, None, None], w)
    elif start == end:
        return (path,w) # Right? <- def need to come back to this
    elif w_key in w.keys():
        if w[w_key][0] == None:
            return ([None, None, None], w)
        else:
            newPath = path[0] + w[w_key][0]
            newDist = path[1] + w[w_key][1]
            newOutdoor = path[2] + w[w_key][2]
            return ([newPath, newDist, newOutdoor],w)       
    else: # Do some DFS
        bestNewPath = None
        bestNewPathDist = best_dist
        outdoorNewPath = None
        for edge in g.edges[start]:
            newStart = edge.get_destination()
            if newStart in path[0]: # this is a cycle
                continue
            newPath, w = get_best_path(g, newStart, end, [path[0] + [newStart],
            path[1]+int(edge.get_total_distance()),path[2]+int(edge.get_outdoor_distance())],
            max_dist_outdoors, best_dist, best_path, w)
            
            if newPath[0] != None and newPath[2] <= max_dist_outdoors and \
            (bestNewPathDist == None or newPath[1] < bestNewPathDist):
                bestNewPath = newPath[0]
                bestNewPathDist = newPath[1]
                outdoorNewPath = newPath[2]
        if bestNewPath == None:
            w[w_key] = [None, None, None]
        else:
            w[w_key] = [bestNewPath[len(path[0]):],bestNewPathDist-path[1],
                outdoorNewPath - path[2]]              
        return ([bestNewPath, bestNewPathDist, outdoorNewPath],w)
    

# Problem 3c: Implement directed_dfs
def directed_dfs(digraph, start, end, max_total_dist, max_dist_outdoors):
    """
    Finds the shortest path from start to end using a directed depth-first
    search. The total distance traveled on the path must not
    exceed max_total_dist, and the distance spent outdoors on this path must
    not exceed max_dist_outdoors.

    Parameters:
        digraph: Digraph instance
            The graph on which to carry out the search
        start: string
            Building number at which to start
        end: string
            Building number at which to end
        max_total_dist: int
            Maximum total distance on a path
        max_dist_outdoors: int
            Maximum distance spent outdoors on a path

    Returns:
        The shortest-path from start to end, represented by
        a list of building numbers (in strings), [n_1, n_2, ..., n_k],
        where there exists an edge from n_i to n_(i+1) in digraph,
        for all 1 <= i < k

        If there exists no path that satisfies max_total_dist and
        max_dist_outdoors constraints, then raises a ValueError.
    """
    begin_path = [[start],0,0]

    ([best_path, best_dist, outdoor_dist],w) = get_best_path(digraph, start, end, 
    begin_path, max_dist_outdoors, None, None, {})
    if best_path == None or best_dist > max_total_dist:
        raise ValueError("No solution found. Sorry")
    else:
        path_string = ['']*len(best_path)
        for i in range(len(best_path)):
            path_string[i] += str(best_path[i])
    print('Best path distance = ' + str(best_dist))
    print('Outdoor distance = ' + str(outdoor_dist))
    return path_string


# ================================================================
# Begin tests -- you do not need to modify anything below this line
# ================================================================

class Ps2Test(unittest.TestCase):
    LARGE_DIST = 99999

    def setUp(self):
        self.graph = load_map("mit_map.txt")

    def test_load_map_basic(self):
        self.assertTrue(isinstance(self.graph, Digraph))
        self.assertEqual(len(self.graph.nodes), 37)
        all_edges = []
        for _, edges in self.graph.edges.items():
            all_edges += edges  # edges must be dict of node -> list of edges
        all_edges = set(all_edges)
        self.assertEqual(len(all_edges), 129)

    def _print_path_description(self, start, end, total_dist, outdoor_dist):
        constraint = ""
        if outdoor_dist != Ps2Test.LARGE_DIST:
            constraint = "without walking more than {}m outdoors".format(
                outdoor_dist)
        if total_dist != Ps2Test.LARGE_DIST:
            if constraint:
                constraint += ' or {}m total'.format(total_dist)
            else:
                constraint = "without walking more than {}m total".format(
                    total_dist)

        print("------------------------")
        print("Shortest path from Building {} to {} {}".format(
            start, end, constraint))

    def _test_path(self,
                   expectedPath,
                   total_dist=LARGE_DIST,
                   outdoor_dist=LARGE_DIST):
        start, end = expectedPath[0], expectedPath[-1]
        self._print_path_description(start, end, total_dist, outdoor_dist)
        dfsPath = directed_dfs(self.graph, start, end, total_dist, outdoor_dist)
        print("Expected: ", expectedPath)
        print("DFS: ", dfsPath)
        self.assertEqual(expectedPath, dfsPath)

    def _test_impossible_path(self,
                              start,
                              end,
                              total_dist=LARGE_DIST,
                              outdoor_dist=LARGE_DIST):
        self._print_path_description(start, end, total_dist, outdoor_dist)
        with self.assertRaises(ValueError):
            directed_dfs(self.graph, start, end, total_dist, outdoor_dist)

    def test_path_one_step(self):
        self._test_path(expectedPath=['32', '56'])

    def test_path_no_outdoors(self):
        self._test_path(
            expectedPath=['32', '36', '26', '16', '56'], outdoor_dist=0)

    def test_path_multi_step(self):
        self._test_path(expectedPath=['2', '3', '7', '9'])

    def test_path_multi_step_no_outdoors(self):
        self._test_path(
            expectedPath=['2', '4', '10', '13', '9'], outdoor_dist=0)

    def test_path_multi_step2(self):
        self._test_path(expectedPath=['1', '4', '12', '32'])

    def test_path_multi_step_no_outdoors2(self):
        self._test_path(
            expectedPath=['1', '3', '10', '4', '12', '24', '34', '36', '32'],
            outdoor_dist=0)

    def test_impossible_path1(self):
        self._test_impossible_path('8', '50', outdoor_dist=0)

    def test_impossible_path2(self):
        self._test_impossible_path('10', '32', total_dist=100)

if __name__ == "__main__":
    os.chdir('C:\Users\Nick\Documents\Code\Data Science PS2')
    g = load_map("mit_map.txt")
    #test = directed_dfs(g, Node('1'), Node('32'), 100000, 10000)
    pdb.set_trace()
    unittest.main()
