ga_route <- ga(type = "permutation", 
               fitness = fitness,
               capacity = 500, 
               demand = route$DEMAND,
               distance =Deta, 
               lower = 1,
               upper = max(route$CUSTNO),
               popSize = 90, 
               pmutation = 0.2,
               monitor = FALSE,
               seed = 123,
               crossover = gaperm_oxCrossover)
            
solution_explain <- fitness_explain(ga_route@solution[1, ], capacity = 500, 
                                    distance = deta, demand = route$DEMAND,crossover=crossover)

solution_explain            


