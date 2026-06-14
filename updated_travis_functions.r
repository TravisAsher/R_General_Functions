# === General Travis R Functions === ----
# __________________________________________________________________________________________________
# Purpose: This code consists of various general functions that I have created in R
# Author: Travis Asher
# __________________________________________________________________________________________________


#' Quality Threshold in Path of Exile
#'
#' @description Computes the minimum quality percent level of an item to be worth picking up based on object shape in grid. Defaults to 2.25. You may specify if you would like a printed description or just the 4-tuple as output. Defaults to 4-tuple output. Intended for use in the Steam game, Path of Exile.
#'
#' @param quality_per_unit 
#' @param descriptive_output 
#'
#' @returns The quality percentage for items of the various block shapes
#' @export
#'
#' @examples
quality_threshold <- function(quality_per_unit=2.25,descriptive_output=FALSE){
  classes <- c(3,4,6,8)
  quality <- 0
  restrict <- c(-1,-1,-1,-1)
  cont_check <- rep(TRUE,length(restrict))
  proceed <- TRUE
  while (proceed == TRUE){
    if (quality >= 20){
      proceed <- FALSE
    }
    margin <- quality/classes
    for (shape in c(1:length(margin))){
      if (cont_check[shape] == TRUE){
        if (margin[shape] >= quality_per_unit){
          restrict[shape] <- quality
          cont_check[shape] <- FALSE
        }
      }
    }
    quality <- quality+1
  }
  for (result in c(1:length(restrict))){
    if (restrict[result] == -1){
      restrict[result] <- "NA"
    }
  }
  if (descriptive_output == FALSE){
    Shape <- c("1x3","1x4","2x2","2x3","2x4")
    Quality_Percent <- c(restrict[1:2],restrict[2],restrict[3:4])
    restrict_frame <- data.frame(Shape,Quality_Percent)
    return(restrict_frame)
  }
  else{
    item_x3 <- paste0("The minimum quality percentage for items one unit wide by three units tall {1x3} you should pick up are {", restrict[1],"%} quality.")
    item_x4_1 <- paste0("The minimum quality percentage for items one unit wide by four units tall {1x4} you should pick up are {", restrict[2],"%} quality.")
    item_x4_2 <- paste0("The minimum quality percentage for items two units wide by two units tall {2x2} you should pick up are {", restrict[2],"%} quality.")
    item_x6 <- paste0("The minimum quality percentage for items two units wide by three units tall {2x3} you should pick up are {", restrict[3],"%} quality.")
    item_x8 <- paste0("The minimum quality percentage for items two units wide by four units tall {2x4} you should pick up are {", restrict[4],"%} quality.")
    return(writeLines(paste(item_x3,item_x4_1,item_x4_2,item_x6,item_x8,sep="\n")))
  }
}




load_GIS_lib <- function(){
  # Loads the most common plotting packages for geomapping in R
  x <- c("rgeos","tmap","ggmap","rgdal","maptools","dplyr","tidyr")
  lapply(x,library,character.only=TRUE)
}




appendwd <- function(addition){
  # Allows a user to set their working directory to a child path by passing a character containing the remaining filepath as input
  setwd(paste(gsub("/","\\\\",getwd()),addition,sep="\\"))
}




selection_by_location <-function(base_layer,selection_layer){
  ###*NOTE: CURRENTLY HAS ISSUES WITH OUTPUT*###
  # Takes a base layer object and selects all objects in a selection layer based on the base layer's ID field. Both input objects
  # must be of one of the three vector SpatialXDataFrame objects (i.e. all spatial data frame types except Grid). If no ID field
  #  exists in the base layer object, one is assigned.
  if (all(lapply(list(base_layer,selection_layer),class) %in% c("SpatialPointsDataFrame", "SpatialLinesDataFrame", "SpatialPolygonsDataFrame"))==FALSE){
    return("base_layer input and selection_layer input must be of the 'SpatialXDataFrame' classes.")
  }
  if (require(rgdal) != TRUE){
    library(rgdal)
  }
  if (is.null(base_layer$ID)){
    for (i in c(1:length(county))){
      base_layer$ID <- i
    }
  }
  input <- 0
  if (input %in% base_layer$ID){
    input_logic <- vector(length = length(selection_layer))
    for (i in (1:length(selection_layer))){input_logic[i] <- gIntersects(base_layer[input+1,],selection_layer[i,])}
    # plot(selection_layer) ###Activate this line to graph the selection with respect to full context. If you add this line back in,
    # be sure to add "add=TRUE" to next line
    plot(selection_layer[input_logic,],col="green")
    if (class(base_layer)=="SpatialPolygonsDataFrame"){
      baselines <- as(base_layer,"SpatialLinesDataFrame")
      plot(baselines[input+1,],col="blue",add=TRUE)}
    else {plot(base_layer[input+1],col="green",add=TRUE)}
  }
}




agg_shared_polypts <- function(input){
  # Collects all points from a SpatialPolygonDataFrame that are not unique to a single polygon
  ne_lin <- as(input,"SpatialLinesDataFrame")
  ne_pts <- as.data.frame(as(ne_lin,"SpatialPointsDataFrame"))
  ne_xcords <- ne_pts[,8]
  ne_ycords <- ne_pts[,9]
  ne_xuniq <- unique(ne_xcords)
  ne_yuniq <- unique(ne_ycords)
  ne_list <- ne_pts[,5]

  # This function collects a list
  long_count <- 0
  linRep_list <- list(mode="numeric",length=0)
  xRep_list <- list(mode="numeric",length=0)
  yRep_list <- list(mode="numeric",length=0)
  for (i in (1:length(unique(ne_xcords)))){
    xtru <- ne_xuniq[i] == ne_xcords
    xtru_val <- ne_xcords[xtru]
    if (length(xtru_val)>length(unique(xtru_val))){
      ytru_val <- ne_ycords[xtru]
      ytru <- ne_yuniq[i] == ne_ycords
      if (length(ytru_val)>length(unique(ytru_val))){
        if (length(ne_list[ne_xuniq[i] == ne_xcords]) == length(unique(ne_list[ne_xuniq[i] == ne_xcords]))){
          long_count <- long_count+1
          linRep_list[long_count] <- ne_list[i]
          xytru <- xtru==TRUE & ytru==TRUE
          xRep_list[long_count] <- unique(ne_xcords[xytru])
          yRep_list[long_count] <- unique(ne_ycords[xytru])
        }
      }
    }
  }

  # Make point list into SpatialPointsDataFrame:
  ne_replist <- cbind(xRep_list,yRep_list)
  ne_replist1 <- as.data.frame(ne_replist,col.names=c("x_cord","y_cord"),row.names = c(1:72))
  ne_replist2 <- data.matrix(ne_replist1, rownames.force = NA)
  ne_spt <- SpatialPoints(ne_replist2)
  ne_rep_pts <- SpatialPointsDataFrame(ne_spt,ne_replist1)
  ne_rep_pts$line <- linRep_list
  return(ne_rep_pts)
}




#' Markov
#'
#' @description Performs a markov chain procedure for an initial vector of length N with square transition matrix of dimension N. Accepts four inputs: the initial vector, the transition matrix, the number of trials desired for iteration, and a variable that determines the type and amount of output: if no 'return_value' is passed to the function, it will return the entire result chain from the Markov process; if 'return_final' is passed 'TRUE', it will return the final probability vector as a result; if 'return_final' is passed with a positive integer 'X', it will return the last 'X' probability vectors from the procedure. The number of 'trials' defaults to 100, and the type of output defaults to the entire chain. Will reject input and provide helpful feedback to fix the issue if any of the following are true: 'trans' is not a square matrix, the length of 'init' does not match the dimension of 'trans'. If a return_position is passed to the function, the output will be the list of probabilities for the given position instead of probability vectors for all positions.
#'
#' @param init 
#' @param trans 
#' @param trials 
#' @param return_final 
#' @param return_position 
#'
#' @returns
#' @export
#'
#' @examples
markov_chain <- function(init,trans,trials=100,return_final=FALSE,return_position=0){

  # Checks to make sure the dimensions of 'init' and 'trans' are acceptable:
  if (dim(trans)[1] != dim(trans)[2]){
    return("'trans' must be a square matrix.")
  }
  if (dim(as.matrix(init))[1] != dim(trans)[1]){
    return("The rank of 'init' must be equal to the dimension of square matrix 'trans'.")
  }

  # Checks to make sure 'init' is a probability vector and that 'trans' is a transition matrix:
  L <- length(init)
  prob_check <- rep(0,L+1)
  prob_check[1] <- sum(init)
  for (i in c(1:L)){
    prob_check[i+1] <- sum(trans[i,])
  }
  prob_check_error <- abs(1-prob_check)
  if (all(prob_check_error<0.001)==FALSE){
    return("All probabality vectors must sum to 1. This also means that all rows of the transition matrix must sum to 1.")
  }

  # Checks 'trials' for validity:
  if (class(trials)=="numeric"){
    if (round(trials)==trials){
      if (trials>0){
      }
      else{
        return("Please enter a positive integer amount of trials to perform.")
      }
    }
    else{
      return("Cannot perform a non-integer amount of trials!")
    }
  }
  else{
    return("'trials' must be an integer.")
  }

  # Checks 'return_final' for validity
  if ((class(return_final) %in% c("logical","numeric")) == FALSE){
    return("'return_final' must either be boolean or an integer.")
  }
  if (class(return_final)=="numeric"){
    if (round(return_final)==return_final){
      if (return_final>0){
      }
      else{
        return("Please enter a positive integer amount of entries to return.")
      }
    }
    else{
      return("Cannot return a non-integer number of entries!")
    }
  }

  # Checks 'return_position' for validity:
  if (return_position==0){
    check_for_position <- FALSE
  }
  else{
    if (class(return_position)=="numeric"){
      if (round(return_position)==return_position){
        if (return_position>0 & return_position<=L){
          check_for_position <- TRUE
        }
        else{
          return("The entry that you would like to return must be a positive integer less than or equal to the length.")
        }
      }
      else{
        return("The position in a vector must be an integer!")
      }
    }
    else{
      return("'return_position' must be a number.")
    }
  }

  # If all conditions are satisfied, performs the following:
  if (check_for_position==TRUE){
    result_list <- c()
  }
  else{
    result_list <- list()
  }
  iter_init <- init
  for (i in c(1:trials)){
    iter_init <- iter_init %*% trans
    if (check_for_position==TRUE){
      result_list[i] <- iter_init[return_position]
    }
    else{
      result_list[[i]]<-as.list(iter_init)
    }
  }
  if (return_final==FALSE){
    return(result_list)
  }
  if (return_final==TRUE){
    return(tail(result_list,1))
  }
  else{
    return(tail(result_list,return_final))
  }
}




#' Sample Markov
#'
#' @description   # This code performs a Markov Chain procedure based on passed initial state vector 'init' and transition matrix 'trans' and then performs a random selection. For each probability vector for a given amount of iterations of the Markov Process, for a given 'limit_state', if the random selection happens to lie within the the range of the cumulative probability of the state prior to the limit_state (or zero if for state 1) and the cumulative probability of the limit_state, then the function returns the number of iterations that have passed as output. Otherwise, the next probability vector resulting from the next iteration of the Markov Process is calculated, and the above repeats. 'limit_state' defaults to state 1. If over one million steps pass without a successful random selection, the function will terminate and return "NULL".
#'
#' @param init 
#' @param trans 
#' @param limit_state 
#'
#' @returns
#' @export
#'
#' @examples
sample_markov <- function(init,trans,limit_state=1){
  # Checks to make sure the dimensions of 'init' and 'trans' are acceptable:
  if (class(init) == "numeric"){
    init <- t(as.matrix(init))
  }
  if (dim(trans)[1] != dim(trans)[2]){
    return("'trans' must be a square matrix.")
  }
  if (dim(init)[2] != dim(trans)[1]){
    return("The rank of 'init' must be equal to the dimension of square matrix 'trans'.")
  }

  # Checks to make sure 'init' is a probability vector and that 'trans' is a transition matrix:
  L <- length(init)
  prob_check <- rep(0,L+1)
  prob_check[1] <- sum(init)
  for (i in c(1:L)){
    prob_check[i+1] <- sum(trans[i,])
  }
  prob_check_error <- abs(1-prob_check)
  if (all(prob_check_error<0.001)==FALSE){
    return("All probabality vectors must sum to 1. This also means that all rows of the transition matrix must sum to 1.")
  }

  # Checks 'limit_state' for validity:
  if (class(limit_state)=="numeric"){
    if (round(limit_state)==limit_state){
      if (limit_state>0){
      }
      else{
        return("Please enter a positive integer amount of limit_state to perform.")
      }
    }
    else{
      return("Cannot perform a non-integer amount of limit_state!")
    }
  }
  else{
    return("'limit_state' must be an integer.")
  }

  # If everything above checks out, the Markov Chain random selection proceeds:
  continue <- TRUE
  count <- 0
  current_state <- init
  while (continue == TRUE){
    if (count > 1e+06){
      continue <- FALSE
    }
    count <- count + 1
    transition_prob <- current_state %*% trans
    cumulative_prob <- transition_prob
    state <- -1
    for (i in c(2:L)){
      cumulative_prob[1,i]<- cumulative_prob[1,i]+cumulative_prob[1,i-1]
    }
    rand_value <- runif(1)
    # The lines below can be included for observation or debugging purposes:
    # print(paste(rand_value," : ",count,sep=""))
    # print(cumulative_prob)
    # writeLines("\n\n")
    if (rand_value >= 0 & rand_value < cumulative_prob[1,1]){
      state <- 1
    }
    for (i in c(2:L)){
      if (rand_value > cumulative_prob[1,i-1] & rand_value < cumulative_prob[1,i]){
        state <- i
      }
    }
    if (state == -1){
      state <- L
    }
    if (state == limit_state){
      continue <- FALSE
    }
    else{
      current_state <- transition_prob
    }
  }
  if (count > 1e+06){
    return(NULL)
  }
  else{
    return(count)
  }
}




#' Generate Random Integers
#'
#' @description Generates 'number' amount of random integers between the values of 'minim' and 'maxum'; these default to 1, 1, and 10 respectively. By default, repetition is accepted, but one can specify to return only unique integers by changing 'rep'. If returning unique integers, one can indicated if they want to also return the actual list of generated integers that included repeat values.
#'
#' @param number 
#' @param minim 
#' @param maxum 
#' @param rep 
#' @param return_dummy 
#'
#' @returns
#' @export
#'
#' @examples
gen_randint <- function(number=1,minim=1,maxum=10,rep=TRUE,return_dummy=FALSE){
  if (rep==TRUE){
    rand_vect <- floor(runif(n = number, min = minim, max = maxum+1))
    return(rand_vect)
  }
  else {
    if (number > 1 + maxum - minim){
      return("You cannot have more unique results than available numbers to choose from.")
    }
    thresh <- 1
    first <- floor(runif(n = 1, min = minim, max = maxum+1))
    build_vect <- c(first)
    dummy_vect <- c(first) ###
    while (thresh < number){
      next_num <- floor(runif(n=1, min = minim, max = maxum+1))
      dummy_vect <- c(dummy_vect,next_num) ###
      if (next_num %in% build_vect == FALSE){
        build_vect <- c(build_vect,next_num)
        thresh <- thresh + 1
      }
    }
    if (return_dummy == TRUE){
      retrn <- list("return_vector" = build_vect, "dummy_vector" = dummy_vect)
      return(retrn)
    }
    retrn <- list("return_vector" = build_vect)
    return(retrn)
  }
}
