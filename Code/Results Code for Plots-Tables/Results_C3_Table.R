
for(s in 1:5){
  tau = 100
  file_name = paste0("Comb12.", s, ".RData")
  load(file_name)
  print(numCompulsory/num_items*100)
  
  #plot(test$lateness_eval, type = "l", ylab = "Mean Lateness",
       #xlab = "Generation")
  #plot(test$cost_eval, type = "l", ylab = "Mean Cost",
       #xlab = "Generation")
  
  gen500 = test$prev_generation_parents[[6]]
  
  #Getting unpacked items
  uniq_sol = unique(gen500[,-1])
  
  unpacked = c()
  
  for(i in 1:nrow(uniq_sol)){
    unpacked[i] = length(which(uniq_sol[i,]==0))
  }
  print(unique(unpacked))



  #Get LR
  #print(round(lp_result$optimum,2))

  tau = 100
  
  
  gen500 = test$prev_generation_parents[[6]]
  constraint_test_500 = constraint_violation_calc(gen500, 
                                                  numCompulsory, item_info,
                                                  bin_capacity_all,total_num_bins)
  
  constraint_test_500$violation
  
  test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                     numCompulsory)
  
  test_eval_lateness_500 = evaluate_lateness(gen500, tau, item_due_dates, bin_ship_all)
  
  
  #plot(test_eval_cost_500~test_eval_lateness_500)
  
  results_unique = unique(cbind(test_eval_cost_500, test_eval_lateness_500))
  results_unique = cbind(1:nrow(results_unique), results_unique)
  
  # Assuming results_unique is a matrix with columns for cost and lateness
  # Convert it to a data frame to use column names more easily
  results_unique <- as.data.frame(results_unique)
  colnames(results_unique) <- c("id", "test_eval_cost_500", "test_eval_lateness_500")  # Adjust names as needed
  
  # Initialize a logical vector to mark non-dominated solutions
  non_dominated <- rep(TRUE, nrow(results_unique))
  
  # Loop over each solution to check if it is dominated by any other solution
  for (i in 1:nrow(results_unique)) {
    for (j in 1:nrow(results_unique)) {
      if (i != j && 
          results_unique$test_eval_cost_500[j] <= results_unique$test_eval_cost_500[i] && 
          results_unique$test_eval_lateness_500[j] <= results_unique$test_eval_lateness_500[i] &&
          (results_unique$test_eval_cost_500[j] < results_unique$test_eval_cost_500[i] || 
           results_unique$test_eval_lateness_500[j] < results_unique$test_eval_lateness_500[i])) {
        non_dominated[i] <- FALSE
        break
      }
    }
  }
  
  # Extract non-dominated solutions
  non_dominated_solutions <- results_unique[non_dominated, ]
  
  # Sort the dataframe by the second column
  non_dominated_solutions <- non_dominated_solutions[order(non_dominated_solutions[, 2]), ]
  
  print(non_dominated_solutions)
}


## COMB 5 PLOTS

tau = 100
file_name = paste0("Comb10.3.RData")
load(file_name)
print(numCompulsory/num_items*100)

#plot(test$lateness_eval, type = "l", ylab = "Mean Lateness",
#xlab = "Generation")
#plot(test$cost_eval, type = "l", ylab = "Mean Cost",
#xlab = "Generation")

gen500 = test$prev_generation_parents[[6]]

#Getting unpacked items
uniq_sol = unique(gen500[,-1])

unpacked = c()

for(i in 1:nrow(uniq_sol)){
  unpacked[i] = length(which(uniq_sol[i,]==0))
}
print(unique(unpacked))



#Get LR
#print(round(lp_result$optimum,2))

tau = 100


gen500 = test$prev_generation_parents[[6]]
constraint_test_500 = constraint_violation_calc(gen500, 
                                                numCompulsory, item_info,
                                                bin_capacity_all,total_num_bins)

constraint_test_500$violation

test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                   numCompulsory)

test_eval_lateness_500 = evaluate_lateness(gen500, tau, item_due_dates, bin_ship_all)


#plot(test_eval_cost_500~test_eval_lateness_500)

results_unique = unique(cbind(test_eval_cost_500, test_eval_lateness_500))
results_unique = cbind(1:nrow(results_unique), results_unique)

# Assuming results_unique is a matrix with columns for cost and lateness
# Convert it to a data frame to use column names more easily
results_unique <- as.data.frame(results_unique)
colnames(results_unique) <- c("id", "test_eval_cost_500", "test_eval_lateness_500")  # Adjust names as needed

# Initialize a logical vector to mark non-dominated solutions
non_dominated <- rep(TRUE, nrow(results_unique))

# Loop over each solution to check if it is dominated by any other solution
for (i in 1:nrow(results_unique)) {
  for (j in 1:nrow(results_unique)) {
    if (i != j && 
        results_unique$test_eval_cost_500[j] <= results_unique$test_eval_cost_500[i] && 
        results_unique$test_eval_lateness_500[j] <= results_unique$test_eval_lateness_500[i] &&
        (results_unique$test_eval_cost_500[j] < results_unique$test_eval_cost_500[i] || 
         results_unique$test_eval_lateness_500[j] < results_unique$test_eval_lateness_500[i])) {
      non_dominated[i] <- FALSE
      break
    }
  }
}

# Extract non-dominated solutions
non_dominated_solutions <- results_unique[non_dominated, ]

# Sort the dataframe by the second column
non_dominated_solutions <- non_dominated_solutions[order(non_dominated_solutions[, 2]), ]

print(non_dominated_solutions)


# Assuming gen500 contains the solutions with each row being an assignment of items to bins
# And non_dominated_solutions contains the non-dominated solutions

# Initialize a vector to store the number of unpacked items for non-dominated solutions
non_dominated_unpacked <- c()

# Loop over each non-dominated solution
for (i in 1:nrow(non_dominated_solutions)) {
  # Extract the corresponding solution from gen500
  solution_index <- non_dominated_solutions$id[i]  # Use the ID column to match the index
  solution <- gen500[solution_index, -1]           # Exclude the first column if it's an ID
  
  # Calculate the number of unpacked items (items assigned to bin 0)
  num_unpacked <- length(which(solution == 0))
  non_dominated_unpacked <- c(non_dominated_unpacked, num_unpacked)
}

# Print the number of unpacked items for each non-dominated solution
print(non_dominated_unpacked)

















