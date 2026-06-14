# R General Functions
Contains my own general functions in R. 


## Current Functions:
quality_threshold(): Computes the minimum quality percent level of an item to be worth picking up based on object shape in grid. Defaults to 2.25. You may specify if you would like a printed description or just the 4-tuple as output. Defaults to 4-tuple output. Intended for use in the Steam game, Path of Exile.
  
load_GIS_lib(): Loads the most common plotting packages for geomapping in R.

appendwd(): Allows a user to set their working directory to a child path by passing a character containing the remaining filepath as input.

selection_by_location(): Takes a base layer object and selects all objects in a selection layer based on the base layer's ID field. Both input objects must be of one of the three vector SpatialXDataFrame objects (i.e. all spatial data frame types except Grid). If no ID field exists in the base layer object, one is assigned.

agg_shared_polygons(): Collects all points from a SpatialPolygonDataFrame that are not unique to a single polygon.

markov_chain(): Performs a markov chain procedure for an initial vector of length N with square transition matrix of dimension N. Accepts four inputs: the initial vector, the transition matrix, the number of trials desired for iteration, and a variable that determines the type and amount of output: if no 'return_value' is passed to the function, it will return the entire result chain from the Markov process; if 'return_final' is passed 'TRUE', it will return the final probability vector as a result; if 'return_final' is passed with a positive integer 'X', it will return the last 'X' probability vectors from the procedure. The number of 'trials' defaults to 100, and the type of output defaults to the entire chain. Will reject input and provide helpful feedback to fix the issue if any of the following are true: 'trans' is not a square matrix, the length of 'init' does not match the dimension of 'trans'. If a return_position is passed to the function, the output will be the list of probabilities for the given position instead of probability vectors for all positions.

sample_markov(): This code performs a Markov Chain procedure based on passed initial state vector 'init' and transition matrix 'trans' and then performs a random selection. For each probability vector for a given amount of iterations of the Markov Process, for a given 'limit_state', if the random selection happens to lie within the the range of the cumulative probability of the state prior to the limit_state (or zero if for state 1) and the cumulative probability of the limit_state, then the function returns the number of iterations that have passed as output. Otherwise, the next probability vector resulting from the next iteration of the Markov Process is calculated, and the above repeats. 'limit_state' defaults to state 1. If over one million steps pass without a successful random selection, the function will terminate and return "NULL".

gen_randint(): Generates 'number' amount of random integers between the values of 'minim' and 'maxum'; these default to 1, 1, and 10 respectively. By default, repetition is accepted, but one can specify to return only unique integers by changing 'rep'. If returning unique integers, one can indicated if they want to also return the actual list of generated integers that included repeat values.
