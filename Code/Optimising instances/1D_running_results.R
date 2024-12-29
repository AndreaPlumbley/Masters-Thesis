
# The 1D MOEA file needs to be run prior to this

library(Matrix)
library(Rglpk)
library(tictoc)
library(foreach)
library(doParallel)

# Set up parallel backend to use multiple processors
num_cores <- 4  # Leave 1 core free
cl <- makeCluster(num_cores)
registerDoParallel(cl)

Sys.time()
foreach(h = 0:3) %dopar% {
  library(Matrix)
  library(Rglpk)
  library(tictoc)
  for(s in 0:9){
    
    # Step 1: Read file into R
    file_name <- paste0("prob_3_B_",h,"_0_", s, ".txt") # Dynamically create the filename
    file_content <- readLines(file_name)
    
    # Step 2: Locate and extract sections
    bins_start <- grep("BINS_SECTIONS", file_content) + 1
    items_start <- grep("ITEMS_SECTIONS", file_content) + 1
    bins_end <- items_start - 2
    items_end <- length(file_content)
    
    bins_lines <- file_content[bins_start:bins_end]
    items_lines <- file_content[items_start:items_end]
    
    # Step 3: Convert sections to data frames
    bins_data <- read.table(text = bins_lines, header = FALSE)
    items_data <- read.table(text = items_lines, header = FALSE)
    
    colnames(bins_data) <- c("BinTypeID", "Capacity", "Cost", "MinNumUse", "MaxNumUse")
    colnames(items_data) <- c("ItemID", "Volume", "Profit", "Compulsory")
    
    
    num_items = nrow(items_data)
    numCompulsory = sum(items_data$Compulsory)
    numNonCompulsory = num_items-numCompulsory
    
    item_weights = items_data$Volume
    
    bin_types = bins_data$BinTypeID
    
    if(length(bin_types)==3){
      bin_ship = c(10, 5, 1)
    }else{
      bin_ship = c(20, 15, 10, 5, 1)
    }
    
    bin_avail = bins_data$MaxNumUse
    bin_capacity = bins_data$Capacity
    bin_cost = bins_data$Cost
    
    all_bins = rep(bin_types, bin_avail)
    
    item_due_dates = round(runif(num_items, 101, 100*(0.6)*max(bin_ship)))
    item_profits = items_data$Profit
    item_info = cbind(1:num_items, item_weights)
    
    bin_capacity_all = c()
    for(i in 1:length(bin_capacity)){
      bin_capacity_all = c(bin_capacity_all, c(rep(bin_capacity[i],bin_avail[i])))
    }
    bin_capacity_all
    
    bin_cost_all = c()
    for(i in 1:length(bin_cost)){
      bin_cost_all = c(bin_cost_all, c(seq(bin_cost[i],bin_cost[i]+5, length.out = bin_avail[i])))
    }
    bin_cost_all
    
    
    bin_ship_all = c()
    for(i in 1:length(bin_types)){
      bin_ship_all = c(bin_ship_all, c(rep(bin_ship[i],bin_avail[i])))
    }
    bin_ship_all
    
    
    total_num_bins = length(bin_cost_all)
    total_bins <- total_num_bins
    
    # Select value for tau - unsure how to decide what this should be 
    tau = 100
    
    ## STEP 1: Create initial population P0
    # Set population size
    N = 200 
    
    
    # Objective function coefficients (minimize cost)
    f_obj <- c(rep(0, num_items * total_bins), bin_cost_all)
    
    # Initialize sparse constraint matrix
    f_con <- Matrix(0, nrow = num_items + total_bins + total_bins, 
                    ncol = total_bins * num_items + total_bins, sparse = TRUE)
    
    # Add item packing constraints - each item must only be packed once
    for (i in 1:numCompulsory) {
      f_con[i, (1:total_bins) + (i-1) * total_bins] <- 1
    }
    
    # Add bin capacity constraints
    for (j in 1:total_bins) {
      for (i in 1:num_items) {
        f_con[num_items + j, j + (i-1) * total_bins] <- item_weights[i]
      }
      f_con[num_items + j, total_bins * num_items + j] <- -bin_capacity_all[j]
    }
    
    # Additional constraints
    nr <- num_items + total_bins
    for (i in (nr + 1):(nr + total_bins)) {
      f_con[i, seq(from = (i - nr), to = total_bins * num_items, by = total_bins)] <- 1
      f_con[i, total_bins * num_items + (i - nr)] <- -num_items
    }
    
    # Right-hand side and directions
    f_dir <- c(rep("==", num_items), rep("<=", total_bins), rep("<=", total_bins))
    f_rhs <- c(rep(1, num_items), rep(0, total_bins), rep(0, total_bins))
    
    # Add bounds [0,1] for linear relaxation
    bounds <- list(lower = list(ind = 1:(total_bins * num_items + total_bins), val = rep(0, total_bins * num_items + total_bins)),
                   upper = list(ind = 1:(total_bins * num_items + total_bins), val = rep(1, total_bins * num_items + total_bins)))
    
    # Solve the LP using Rglpk (relaxation, not binary)
    tic()
    lp_result <- Rglpk_solve_LP(f_obj, f_con, f_dir, f_rhs, bounds = bounds, types = rep("C", length(f_obj)), max = FALSE)
    lp_toc = toc()
    
    # Check the status and retrieve the solution if feasible
    if (lp_result$status == 0) {
      x_solution <- matrix(lp_result$solution[1:(total_bins * num_items)], nrow = num_items, byrow = TRUE)
      y_solution <- lp_result$solution[(total_bins * num_items + 1):(total_bins * num_items + total_bins)]
      #  Output the results
      list(x_solution = x_solution, y_solution = y_solution, objective_value = lp_result$optimum)
    } else {
      cat("No feasible solution found.\n")
    }
    
    tolerance <- 1e-10  # You can adjust this threshold if necessary
    y_solution <- ifelse(abs(y_solution) < tolerance, 0, y_solution)
    x_solution <- ifelse(abs(x_solution) < tolerance, 0, x_solution)
    
    tester = get_init_pop_for_GA(x_solution, y_solution, total_num_bins,
                                 pop_size = 200, num_items = num_items, 
                                 numCompulsory = numCompulsory)
    
    
    ## STEP 1: Create initial population P0
    # Set population size
    N = 200 
    
    # Generate initial pop
    set.seed(2024)
    P0 = initial_pop(num_items, total_num_bins, N)
    
    if(s<=4){
      P0$initial_pop_sol = cbind(1:N,tester)
    }
    
    late_P0 = evaluate_lateness(P0$initial_pop_sol, tau, item_due_dates, bin_ship_all)
    
    cost_P0 = evaluate_cost(P0$initial_pop_sol, bin_cost_all, item_profits = item_profits, 
                            numCompulsory = numCompulsory)
    
    # Now do a constraint violation calculation:
    
    get_constraint_vals = constraint_violation_calc(P0$initial_pop_sol, numCompulsory, item_info, 
                                                    bin_capacity_all, total_num_bins)
    
    constraint_vals_P0 = get_constraint_vals$violation
    
    
    P0_obj = cbind(c(1:N), late_P0, cost_P0, constraint_vals_P0)
    
    # Now get the ranks of each of the solutions using fast_non_dom_sort()
    
    ranks = fast_non_dom_sort(P0_obj)$Rank
    
    crowding_distance = c()
    for(i in unique(ranks)){
      crowding_distance_i = crowding_dist_assign(P0_obj[which(ranks==i),-4])
      
      if(is.null(nrow(crowding_distance_i))){
        crowding_distance[which(ranks == i)] = crowding_distance_i[2]
      }else{
        crowding_distance[which(ranks == i)] = crowding_distance_i[,2]
      }
    }
    
    Q0 = make_new_pop(P0$initial_pop_sol, ranks, constraint_vals_P0, total_num_bins, 
                      crowd_distance = rep(0, nrow(P0$initial_pop_sol)) )
    
    Q0 = cbind(c((N+1):(2*N)), Q0)
    
    tic()
    t1 = Sys.time()
    test = run_t_generations(500, P0$initial_pop_sol, 
                             Q0, 
                             m = 2,
                             bin_costs = bin_cost_all, numCompulsory = numCompulsory,
                             item_info = item_info, bin_capacitys = bin_capacity_all,
                             totalNumBins = total_num_bins, 
                             item_profits = item_profits, item_due_dates = item_due_dates,
                             bin_ship_time = bin_ship_all,
                             gen_store = c(50, 100, 200, 300, 400, 500))
    toc_end = toc()
    t2 = Sys.time()
    
    result_filename <- paste0("R_C0_prob_3_B", h, "_0_", s, ".RData")
    save(num_items, numCompulsory, item_weights, bin_types, bin_ship, bin_avail,
         bin_capacity, bin_cost, all_bins, item_due_dates, item_profits, item_info, 
         bin_capacity_all, bin_cost_all, bin_ship_all, total_num_bins, total_bins,
         test, lp_result, file = result_filename)  # Save both objects in the same file
    # Store time in a text file
    time_filename <- paste0("R_C0_prob_3_B", h, "_0_", s, ".txt")
    writeLines(c(paste("lp_toc", lp_toc), paste("t1:", t1), paste("t2:", t2), 
                 paste("toc_end:", toc_end)), con = time_filename)
    
  }
}
stopCluster(cl)