
#Run the 2D MOEA for load and cost file first to get necessary functions

set.seed(2024)
library(foreach)
library(doParallel)

num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)

Sys.time()

foreach(h = 6:10) %dopar% {
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(tictoc)
  library(lpSolve)
  library(Rglpk)
  library(Matrix)
  bin_types = data.frame(bin_types = c(0,1,3),
                         bin_avail = c(3,3,3),
                         bin_cost = c(100,500,2000),
                         bin_hs = c(200,300,400),
                         bin_ws = c(200,300,400),
                         bin_ship = c(15,8,1)
  ) %>% mutate(bin_area = bin_hs*bin_ws)
  
  num_bins = nrow(bin_types)
  all_bins = rep(bin_types$bin_types, bin_types$bin_avail)
  
  bin_heights_all = c()
  for(i in 1:nrow(bin_types)){
    bin_heights_all = c(bin_heights_all, c(rep(bin_types$bin_hs[i], bin_types$bin_avail[i])))
  }
  
  bin_widths_all = c()
  for(i in 1:nrow(bin_types)){
    bin_widths_all = c(bin_widths_all, c(rep(bin_types$bin_ws[i], bin_types$bin_avail[i])))
  }
  
  bin_area_all = c()
  for(i in 1:nrow(bin_types)){
    bin_area_all = c(bin_area_all, c(rep(bin_types$bin_area[i], bin_types$bin_avail[i])))
  }
  
  bin_cost_all = c(seq(100, 105, length.out = bin_types$bin_avail[1]),
                   seq(500, 505, length.out = bin_types$bin_avail[2]),
                   seq(2000, 2005, length.out = bin_types$bin_avail[2]))
  
  total_num_bins = length(bin_cost_all)
  total_bins = sum(bin_types$bin_avail)
  
  prop_comp = 0.8

    file_name = paste0("Comb18.6.txt")
    file_content = readLines(file_name)
    
    items_start = grep("50", file_content)+1
    items_end = length(file_content)
    
    item_lines = file_content[items_start:items_end]
    
    items_data = read.table(text = item_lines, header = FALSE)
    
    num_items = nrow(items_data)
    items_data = cbind(1:num_items, items_data)
    numCompulsory = prop_comp*num_items
    numNonCompulsory = num_items - numCompulsory
   
    
    item_widths = items_data[,3]
    item_heights = items_data[,2]
    item_area = item_widths*item_heights
   
    item_profits <- runif(num_items, 0.1, 0.3)*(item_area)
    
    item_weights = item_area*0.1
    item_weights = runif(50, 0,100)
   
    item_info <- cbind(c(1:num_items),item_heights, item_widths, item_weights)

  # # 
  # # ## STEP 1: Create initial population P0
  # # # Set population size
    N = 200
    non_comp_items_profit = item_profits[(numCompulsory+1):num_items]
   # 
   # # Objective function coefficients (minimize cost)
    f_obj <- c(rep(0, num_items * total_bins), bin_cost_all, non_comp_items_profit)
   # # Constraint matrices
    f_con <- Matrix(0, nrow = num_items + total_bins + total_bins,
                    ncol = total_bins * num_items + total_bins + (num_items-numCompulsory),
                    sparse = TRUE)
    
    for (i in 1:numCompulsory) {
      f_con[i, (1:total_bins) + (i-1) * total_bins] <- 1
    }
    
    for (i in (numCompulsory+1):num_items) {
      f_con[i, (1:total_bins) + (i-1) * total_bins] <- 1
    }
   # 
    f_con[(numCompulsory+1):num_items, (ncol(f_con)-(num_items-numCompulsory)+1):ncol(f_con)] = diag(1,num_items-numCompulsory)
   # 
    # Add bin capacity constraints
    for (j in 1:total_bins) {
      for (i in 1:num_items) {
        f_con[num_items + j, j + (i-1) * total_bins] <- item_area[i]
      }
      f_con[num_items + j, total_bins * num_items + j] <- -bin_area_all[j]
    }
   # 
    # Additional constraints from Code 1
    nr = num_items+total_bins
    for(i in (nr + 1):(nr + total_bins)) {
      f_con[i, seq(from = (i - nr), to = total_bins * num_items, by = total_bins)] <- 1
      f_con[i, total_bins * num_items + (i - nr)] <- -num_items
    }
    
    # Right-hand side and directions
    f_dir <- c(rep("==", numCompulsory), rep("==", (num_items - numCompulsory)), rep("<=", total_bins), rep("<=", total_bins))
    f_rhs <- c(rep(1, num_items), rep(0, total_bins), rep(0,total_bins))
   # 
    # Add bounds [0,1] for linear relaxation
    bounds <- list(lower = list(ind = 1:(total_bins * num_items + total_bins), val = rep(0, total_bins * num_items + total_bins)),
                   upper = list(ind = 1:(total_bins * num_items + total_bins), val = rep(1, total_bins * num_items + total_bins)))
    dim(f_con)
    length(f_obj)
    length(f_rhs)
    
    # Solve the LP using Rglpk (relaxation, not binary)
    tic()
    lp_result <- Rglpk_solve_LP(f_obj, f_con, f_dir, f_rhs, bounds = bounds, types = rep("C", length(f_obj)), max = FALSE)
    lp_toc = toc()
   # 
   # # # Get the solution if feasible
     if (lp_result$status == 0) {
      x_solution <- matrix(lp_result$solution[1:(total_bins * num_items)], nrow = num_items, byrow = TRUE)
       y_solution <- lp_result$solution[(total_bins * num_items + 1):(total_bins * num_items + total_bins)]
       nonCompulsory_unloaded <- lp_result$solution[(total_bins * num_items + total_bins + 1):length(lp_result$solution)]
       # Output the results
       list(x_solution = x_solution, y_solution = y_solution,
            nonCompulsory_unloaded = nonCompulsory_unloaded,objective_value = lp_result$optimum)
     } else {
       cat("No feasible solution found.\n")
     }
     
     tolerance <- 1e-10  # You can adjust this threshold if necessary
     y_solution <- ifelse(abs(y_solution) < tolerance, 0, y_solution)
     x_solution <- ifelse(abs(x_solution) < tolerance, 0, x_solution)
     
     tester = get_init_pop_for_GA(x_solution, y_solution, total_num_bins,
                                  pop_size = 200, num_items = num_items,
                                  numCompulsory = numCompulsory)
    
    
     # Set population size
     N = 200 
     
     # Generate initial pop
     set.seed(2024)
     
     P0 = initial_pop(num_items, total_num_bins, N)
     # The following line is uncommented when the LP start point must be used. 
     #P0$initial_pop_sol = cbind(1:N,tester)
     P0_rotation = initial_pop_rotations(num_items, N)
     
     
     cost_P0 = evaluate_cost(P0$initial_pop_sol, bin_cost_all, item_profits = item_profits, 
                             numCompulsory = numCompulsory)
     
     constraint_and_corner_P0 = constraint_violation_calc(solution_pop=P0$initial_pop_sol, 
                                                          solution_pop_rotations = P0_rotation$initial_rotation,
                                                          numCompulsory = numCompulsory, 
                                                          item_info = item_info, 
                                                          bin_heights = bin_heights_all, 
                                                          bin_widths = bin_widths_all,
                                                          totalNumBins = total_num_bins)
     
     constraint_vals_P0 = constraint_and_corner_P0$violation
     
     load_imbalance_P0 = evaluate_load_imbalance(solution_pop=P0$initial_pop_sol,
                                                 solution_pop_rotations = P0_rotation$initial_rotation,
                                                 item_info = cbind(item_info, item_weights),
                                                 bin_height = bin_heights_all, 
                                                 bin_width = bin_widths_all,
                                                 bottom_corners = constraint_and_corner_P0$bottom_corners)
     
     P0_obj = cbind(c(1:N), cost_P0, load_imbalance_P0, constraint_vals_P0)
     
     # Now get the ranks of each of the solutions using fast_non_dom_sort()
     fast_non_results = fast_non_dom_sort(P0_obj)
     
     ranks = fast_non_results$Rank
     
     crowding_distance = c()
     for(i in unique(ranks)){
       #print(which(ranks==i))
       crowding_distance_i = crowding_dist_assign(P0_obj[which(ranks==i),-4])
       
       if(is.null(nrow(crowding_distance_i))){
         crowding_distance[which(ranks == i)] = crowding_distance_i[2]
       }else{
         crowding_distance[which(ranks == i)] = crowding_distance_i[,2]
       }
     }
     
     
     Q0 = make_new_pop(parent_solution_population = P0$initial_pop_sol, 
                       parents_rotations = P0_rotation$initial_rotation, 
                       ranks = ranks, 
                       constraint_violations = constraint_vals_P0,
                       totalNumBins = total_num_bins, 
                       crowd_distance = crowding_distance)
     
     
     Q0$offspring_mutate = cbind(c((N+1):(2*N)), Q0$offspring_mutate)
     Q0$rotation_offspring = cbind(c((N+1):(2*N)), Q0$rotation_offspring)
     
     tic()
     t1 = Sys.time()
     test = run_t_generations(500, P0$initial_pop_sol, 
                              Q0$offspring_mutate, 
                              P0_rotation$initial_rotation,
                              Q0$rotation_offspring,
                              m = 2,
                              bin_costs = bin_cost_all, numCompulsory = numCompulsory,
                              item_info = item_info, bin_heights = bin_heights_all,
                              bin_widths = bin_widths_all,
                              totalNumBins = total_num_bins, 
                              item_profits = item_profits,
                              item_weights = item_weights,
                              gen_store = c(50, 100, 200, 300, 400, 500), h=1)
     run_t_time = toc()
     t2 = Sys.time()
    
     result_filename = paste0("Results_Combination_rerun_load.RData")
     save(num_items, numCompulsory, bin_types, all_bins, item_heights, item_widths, item_weights,
          bin_cost_all, bin_ship_all, total_num_bins, bin_heights_all, bin_widths_all,
          item_info, item_profits, lp_result, test, file = result_filename)  # Save the test object
    
     # Store time in a text file
     time_filename = paste0("Results_Combination_rerun_load.txt")
     writeLines(c(paste("lp_toc", lp_toc), paste("t1:", t1), paste("t2:", t2), 
                  paste("toc_end:", run_t_time)), con = time_filename)
     
  
}
stopCluster(cl)
Sys.time()