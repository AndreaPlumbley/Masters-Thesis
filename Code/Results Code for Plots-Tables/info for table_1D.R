
num_uniq = c()

for(prob in 1:3){
for(let in c("A", "B")){
    for(instance in 0:9){
      
      item = 3
      
      file_name = paste0("R_C0_prob_", prob, "_", let, item, "_0_", instance,".RData")
      load(file_name)
      
      tau = 100
      
      lp_result$optimum
      
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
      num_uniq = c(num_uniq, nrow(non_dominated_solutions))
      print(non_dominated_solutions)
    }
}
}

length(which(num_uniq ==1))
length(which(num_uniq ==2))
length(which(num_uniq ==3))
length(which(num_uniq ==4))
length(which(num_uniq >=5))




  