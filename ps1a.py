###########################
# 6.0002 Problem Set 1a: Space Cows 
# Name: Nick Link
# Collaborators: Me, Myself and I
# Time:

from ps1_partition import get_partitions
import time
import os
import pdb
import operator

#================================
# Part A: Transporting Space Cows
#================================

# Problem 1
def load_cows(filename):
    """
    Read the contents of the given file.  Assumes the file contents contain
    data in the form of comma-separated cow name, weight pairs, and return a
    dictionary containing cow names as keys and corresponding weights as values.

    Parameters:
    filename - the name of the data file as a string

    Returns:
    a dictionary of cow name (string), weight (int) pairs
    """
    # TODO: Your code here
    with open(filename) as f:
        data = f.read().split('\n')
    data2 = dict([[line.split(',')[0],int(line.split(',')[1])] for line in data])
    data = [line.split(',') for line in data]
    data = dict(data)
    #print(data2)
    for k in data.keys():
        data[k] = int(data[k])
    #print(data)

    return data

# Problem 2
def greedy_cow_transport(cows,limit=10):
    """
    Uses a greedy heuristic to determine an allocation of cows that attempts to
    minimize the number of spaceship trips needed to transport all the cows. The
    returned allocation of cows may or may not be optimal.
    The greedy heuristic should follow the following method:

    1. As long as the current trip can fit another cow, add the largest cow that will fit
        to the trip
    2. Once the trip is full, begin a new trip to transport the remaining cows

    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    # TODO: Your code here
    trips = []
    cows = sorted(cows.items(), key=operator.itemgetter(1), reverse = True)
    newTrip=['blank']   
    while (len(cows)>0 and newTrip!=[]):
        newTrip=[]
        spaceOnTrip = limit
        for cow in cows:
            if cow[1] <= spaceOnTrip:
                cows.remove(cow)
                spaceOnTrip -= cow[1]
                newTrip.append(cow[0])
        trips.append(newTrip)
    if newTrip == []:
        print('could not fill all of the space ships. Sorry cows')
    return trips
        


# Problem 3
def brute_force_cow_transport(cows,limit=10):
    """
    Finds the allocation of cows that minimizes the number of spaceship trips
    via brute force.  The brute force algorithm should follow the following method:

    1. Enumerate all possible ways that the cows can be divided into separate trips 
        Use the given get_partitions function in ps1_partition.py to help you!
    2. Select the allocation that minimizes the number of trips without making any trip
        that does not obey the weight limitation
            
    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    "C:\Users\Nick\Documents\Code\Data Science PS1\ps1_partition.py"
    minNum = len(cows)
    bestParty=None
    cow_partitions = get_partitions(cows.keys())
    for party in cow_partitions:
        partyGood = True
        #pdb.set_trace()
        for spaceShip in party:
            shipSum=0
            for cow in spaceShip:
                shipSum += cows[cow]
            if shipSum > limit: # ship don't fit!
                partyGood = False
        if partyGood and (len(party) < minNum):
            bestParty = party
            minNum = len(party)
    return bestParty
        
# Problem 4
def compare_cow_transport_algorithms():
    """
    Using the data from ps1_cow_data.txt and the specified weight limit, run your
    greedy_cow_transport and brute_force_cow_transport functions here. Use the
    default weight limits of 10 for both greedy_cow_transport and
    brute_force_cow_transport.
    
    Print out the number of trips returned by each method, and how long each
    method takes to run in seconds.

    Returns:
    Does not return anything.
    """
    # TODO: Your code here
    os.chdir('C:\Users\Nick\Documents\Code\Data Science PS1') #where should this go?
    cows = load_cows('ps1_cow_data.txt')
    start = time.time()
    trips_brute = brute_force_cow_transport(cows,limit=10)
    end = time.time()
    print('brute force took ' + str(end - start) + ' time and had ' + str(len(trips_brute)) + 'ships')
    start = time.time()
    trips_greedy = greedy_cow_transport(cows, 10)
    end = time.time()
    print('greedy alg took ' + str(end - start) + ' time and had ' + str(len(trips_greedy)) + 'ships')
    print(trips_brute)
    print(trips_greedy)
    print(cows)
    pass
    
compare_cow_transport_algorithms()
