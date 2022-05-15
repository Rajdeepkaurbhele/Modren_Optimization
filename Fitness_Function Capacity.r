
library(tidyverse)
library(ggrepel)
library(GA)
library("readxl")
library(hash)
route<<-read_excel("C:/Users/sanjay/Desktop/Modren optimization/CVRP.xlsx")
#route<<-read_excel("D:/CVRP.xlsx")
Deta <<- dist(route[ , 2:3], upper = T, diag = T) %>% as.matrix()
head(Deta)


fitness <- function(x, capacity, demand, distance, ...){
  
 Load <- capacity
  visited_spot <- 1
  vehicle_num <- -1
  total_demand <- 1
  routeList <- list()
  routeKV <-list()
  v_count <- 0 # Number of Vehicle counter
  
  for (i in x) {
    
    First_spot <- i
    
    if ( Load > demand[First_spot]) {
      
      # Go to the spot
      visited_spot <- c(visited_spot, First_spot)
      Load <-  Load - demand[ First_spot ]
    } 
    else {
      # Go back to depot
      Load <- capacity
      visited_spot <- c(visited_spot, 1)
      #vehicle_num <- vehicle_num + 1
      
      # Go to the spot 
      visited_spot <- c(visited_spot, First_spot)
      Load <-  Load - demand[ First_spot ]
    }
  }
  
  visited_spot <- c(visited_spot, 1)
  
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  for(i in visited_spot){
    if(i!=1){
      routeList <- append(routeList, i)
      total_demand <- total_demand + demand[[i]]
    }
    else{
      vehicle_num <- vehicle_num + 1
      routeKV[[sprintf("V%d",vehicle_num)]] = total_demand  #v1:120 
      routeList <- list()
      total_demand=0
    }
  }
  
  #for(r in routeKV){
  # for(nodes in r){
  #  total_demand <- c(total_demand , demand[[nodes]])
  #}
  #}
    
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  return(-total_distance)
}

fitness_explain <- function(x, capacity, demand, distance, ...){
  
  Load <- capacity
  visited_spot <- 1
  vehicle_num <- -1
  total_demand <- 1
  routeList <- list()
  routeKV <-list()
  v_count <- 0 # Number of Vehicle counter
  
  for (i in x) {
    
    First_spot <- i
    
    if ( Load > demand[First_spot]) {
      
      # Go to the spot
      visited_spot <- c(visited_spot, First_spot)
      Load <-  Load - demand[ First_spot ]
    } 
    else {
      # Go back to depot
      Load <- capacity
      visited_spot <- c(visited_spot, 1)
      #vehicle_num <- vehicle_num + 1
      
      # Go to the spot 
      visited_spot <- c(visited_spot, First_spot)
      Load <-  Load - demand[ First_spot ]
    }
  }
  
  visited_spot <- c(visited_spot, 1)
  
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  for(i in visited_spot){
    if(i!=1){
      routeList <- append(routeList, i)
      total_demand <- total_demand + demand[[i]]
    }
    else{
      vehicle_num <- vehicle_num + 1
      routeKV[[sprintf("V%d",vehicle_num)]] = total_demand  #v1:120 
      routeList <- list()
      total_demand=0
    }
  }
  
  #for(r in routeKV){
   # for(nodes in r){
    #  total_demand <- c(total_demand , demand[[nodes]])
    #}
  #}
  
  result <- list(route = visited_spot,
                 total_distance = total_distance,
                 #vehicle_num = length(names(routeKV)),
                 vehicle_num = vehicle_num,
                 total_demand = routeKV)  
  
  return(result)
}

#To call this function, you must pass it on a GA produced solution. 
#For example:
#results <- runGA(problem = "tsp")
#solution <- getBestSolution()
# plotTSPSolution(solution)
plotTSPSolution<-function(solution){
  data("eurodist", package = "datasets")
  mds <- cmdscale(eurodist)
  x <- mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
         col = "light gray")
  tour <- solution[1, ]
  tour <- c(tour, tour[1])
  n <- length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
         length = 0.15, angle = 25, col = "steelblue", lwd = 2)
  text(x, y, labels(eurodist), cex=0.8)
}


