
overall_time = list()
times = c()
count = 1
for(i in c(27, 29, 33, 35)){
  for(j in 1:5){
    file_name = paste0("Results_Combination", i,".",j, ".txt")
    file_content = readLines(file_name)
    info_time = tail(file_content, n =1)
    number <- as.numeric(gsub(".*: ([0-9.]+) sec.*", "\\1", info_time))
    times[count] = number
    count = count +1
  }
}

overall_time[[3]] = times/60

boxplot(overall_time,
        names = c("20", "50", "100", "200"),  # Labels for each plot
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),  # Colors for each boxplot
        las = 1,
        xlab ="Number of items",
        ylab = "Time (minutes)")
boxplot(overall_time[[1]])


tau = 100
file_name = paste0("Results_Comb3.2.RData")
load(file_name)

gen500 = test$prev_generation_parents[[6]]
gen500_rotations = test$prev_generation_parents_rotations[[6]]

## Gen 500 

constraint_test_500 = constraint_violation_calc(gen500, gen500_rotations,numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_500$violation

test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_500



test_eval_lateness_500 = evaluate_lateness(gen500, tau, item_due_dates, bin_ship_all)

test_eval_lateness_500
plot(test_eval_cost_500~test_eval_lateness_500)
lp_result$optim
  (sum(item_heights*item_widths)/(200*200))*100

identify_non_dominated <- function(cost, lateness) {
  results_unique <- unique(cbind(cost, lateness))
  results_unique <- cbind(1:nrow(results_unique), results_unique)
  results_unique <- as.data.frame(results_unique)
  colnames(results_unique) <- c("id", "cost", "lateness")
  
  non_dominated <- rep(TRUE, nrow(results_unique))
  for (i in 1:nrow(results_unique)) {
    for (j in 1:nrow(results_unique)) {
      if (i != j && 
          results_unique$cost[j] <= results_unique$cost[i] &&
          results_unique$lateness[j] <= results_unique$lateness[i] &&
          (results_unique$cost[j] < results_unique$cost[i] || 
           results_unique$lateness[j] < results_unique$lateness[i])) {
        non_dominated[i] <- FALSE
        break
      }
    }
  }
  
  results_unique[non_dominated, ]
}

non_dom_500 = identify_non_dominated(test_eval_cost_500, test_eval_lateness_500)
non_dom_500

head(test_eval_lateness_500)

plot(test_eval_cost_500~test_eval_lateness_500, pch = 16, cex = 1.2, col = "darkgreen"
     , xlab = "Maximum Lateness",
     ylab = "Cost", cex.lab = 1.5)

plot(non_dom_500$cost~non_dom_500$lateness, pch = 16, cex = 1.2, col = "darkgreen"
       , xlab = "Maximum Lateness",
     ylab = "Cost", cex.lab = 1.5)

plot(test$load_imbalance_eval, type = "l", xlab = "Generation", ylab = "Mean Lateness")
plot(test$cost_eval, type = "l", xlab = "Generation", ylab = "Mean Cost")

### ==========================================================


gen50 = test$prev_generation_parents[[1]]
gen100 = test$prev_generation_parents[[2]]
gen200 = test$prev_generation_parents[[3]]
gen300 = test$prev_generation_parents[[4]]
gen400 = test$prev_generation_parents[[5]]
gen500 = test$prev_generation_parents[[6]]


gen50_rotations = test$prev_generation_parents_rotations[[1]]
gen100_rotations = test$prev_generation_parents_rotations[[2]]
gen200_rotations = test$prev_generation_parents_rotations[[3]]
gen300_rotations = test$prev_generation_parents_rotations[[4]]
gen400_rotations = test$prev_generation_parents_rotations[[5]]
gen500_rotations = test$prev_generation_parents_rotations[[6]]

total_bins = total_num_bins
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

item_area = item_heights*item_widths
sum(item_area)/(200*200)*100

sum(y_solution*bin_cost_all)
## Gen 50 

constraint_test_50 = constraint_violation_calc(gen50, gen50_rotations,
                                               numCompulsory, item_info,
                                               bin_heights_all,
                                               bin_widths_all,total_num_bins)

constraint_test_50$violation

test_eval_cost_50 = evaluate_cost(gen50, bin_cost_all, item_profits, 
                                  numCompulsory)

test_eval_cost_50


test_eval_lateness_50 = evaluate_lateness(gen50, tau, item_due_dates, bin_ship_all)
test_eval_lateness_50

## Gen 100 

constraint_test_100 = constraint_violation_calc(gen100, gen100_rotations,
                                                numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_100$violation

test_eval_cost_100 = evaluate_cost(gen100, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_100

test_eval_lateness_100 = evaluate_lateness(gen100, tau, item_due_dates, bin_ship_all)

test_eval_lateness_100


## Gen 200 

constraint_test_200 = constraint_violation_calc(gen200, gen200_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_200$violation

test_eval_cost_200 = evaluate_cost(gen200, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_200

test_eval_lateness_200 = evaluate_lateness(gen200, tau, item_due_dates, bin_ship_all)

test_eval_lateness_200


## Gen 300 

constraint_test_300 = constraint_violation_calc(gen300, gen300_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_300$violation

test_eval_cost_300 = evaluate_cost(gen300, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_300



test_eval_lateness_300 = evaluate_lateness(gen300, tau, item_due_dates, bin_ship_all)

test_eval_lateness_300


## Gen 400 

constraint_test_400 = constraint_violation_calc(gen400, gen400_rotations,
                                                numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_400$violation

test_eval_cost_400 = evaluate_cost(gen400, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_400



test_eval_lateness_400 = evaluate_lateness(gen400, tau, item_due_dates, bin_ship_all)

test_eval_lateness_400

## Gen 500 

constraint_test_500 = constraint_violation_calc(gen500, gen500_rotations,numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_500$violation

test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_500



test_eval_lateness_500 = evaluate_lateness(gen500, tau, item_due_dates, bin_ship_all)

test_eval_lateness_500

lp_result$optimum


item_area = item_heights*item_widths
sum(item_area)


plot(test_eval_cost_50~test_eval_lateness_50, pch = 16, cex = 1.2, col = "red", 
     xlim=c(450, 1500), ylim = c(0, 28000), xlab = "Maximum Lateness",
     ylab = "Cost Objective", cex.lab = 1.5)
points(test_eval_cost_100~test_eval_lateness_100, pch = 16,cex = 1.2, col = "blue")
points(test_eval_cost_200~test_eval_lateness_200, pch = 16,cex = 1.2, col = "orange")
points(test_eval_cost_300~test_eval_lateness_300, pch = 16,cex = 1.2, col = "brown")
points(test_eval_cost_400~test_eval_lateness_400, pch = 16, cex = 1.2,col = "darkgreen")
points(test_eval_cost_500~test_eval_lateness_500, pch = 16, cex = 1.2,col = "green")
legend("topleft", legend=c("50 generations", "100 generations", "200 generations", "300 generations", "400 generations", "500 generations"), 
       col=c("red", "blue", "orange","brown", "darkgreen","green"), pch=16, pt.cex = 1.5)


plot(test_eval_cost_500~test_eval_lateness_500)

library(ggplot2)

get_packing_sols = function(parents, parent_rotations, bottom_corners, 
                            item_info, bin_heights, bin_widths){
  
  uniq_sols = unique(parents[,-1])
  uniq_sol = uniq_sols[1,]
  
  matches = apply(parents[, -1], 1, function(x) all(x == uniq_sol))
  index = which(matches)[1]
  
  rotation_this = parent_rotations[index,]
  this_item_info = update_info_rotate(item_info, rotation_this[-1])
  
  uniq_corners = bottom_corners[[index]]
  bins = as.numeric(unique(uniq_corners[,4]))
  
  plots = list()
  plot_num = 1
  for(j in bins){
    
    # Filter the unique corners for the current bin
    uniq_corners_j = uniq_corners[uniq_corners[,4] == j,]
    
    # Handle cases with no rows
    if(is.null(nrow(uniq_corners_j)) || nrow(uniq_corners_j) == 0){
      uniq_corners_j = as.data.frame(t(uniq_corners_j))
      item_info_j = this_item_info[this_item_info[,1] %in% uniq_corners_j[,1],]
      item_info_j = as.data.frame(t(item_info_j))
      item_info_j$x = as.numeric(uniq_corners_j[,2])
      item_info_j$y = as.numeric(uniq_corners_j[,3])
    } else {
      item_info_j = this_item_info[this_item_info[,1] %in% uniq_corners_j[,1],]
      item_info_j = as.data.frame(item_info_j)
      item_info_j$x = as.numeric(uniq_corners_j[,2])
      item_info_j$y = as.numeric(uniq_corners_j[,3])
    }
    
    # Assign column names
    colnames(item_info_j) = c("item_id", "height", "width", "x", "y")
    
    # Retrieve the bin dimensions
    bin_height = bin_heights[j]
    bin_width = bin_widths[j]
    
    # Create a data frame for the bin outline
    bin_outline = data.frame(xmin = 0, xmax = bin_width, ymin = 0, ymax = bin_height)
    
    # Plot for the current bin with the bin's full area
    plots[[plot_num]] = ggplot() +
      # Draw the full bin area as a rectangle using bin_outline data
      geom_rect(data = bin_outline, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                fill = "white", color = "blue", linetype = "dashed", linewidth = 1) +
      # Draw each item as a rectangle inside the bin
      geom_rect(data = item_info_j, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), 
                fill = "lightgrey", color = "black") +
      # Add item labels
      geom_text(data = item_info_j, aes(x = x + width / 2, y = y + height / 2, label = item_id), 
                color = "black", size = 5) +
      # Fix the aspect ratio and add minimal theme
      coord_fixed() +
      theme_minimal() +
      # Label the axes and remove the legend
      labs(x = "X Coordinate (Width)",
           y = "Y Coordinate (Height)") +
      theme(legend.position = "none")
    
    plot_num = plot_num + 1
  }
  
  return(plots)
}

gen500 = rep(9, 20)
gen500_rotations = rep(1, 20)

gen500 = rbind(gen500, gen500)
gen500_rotations = rbind(gen500_rotations, gen500_rotations)

gen500 = cbind(1:2, gen500)
gen500_rotations = cbind(1:2, gen500_rotations)

get_corners = constraint_violation_calc(gen500, gen500_rotations, numCompulsory,
                                        item_info, bin_heights_all, bin_widths_all,
                                        total_num_bins)

test_plots = get_packing_sols(gen500, gen500_rotations, constraint_test_500$bottom_corners, 
                              item_info, bin_heights_all, bin_widths_all)

test_plots

c

unique(test_eval_cost_500)
which(test_eval_lateness_500==1378)

uniq_solutions = unique(gen500[,-1])

sol_uniq = uniq_solutions[1,]

bins_used_uniq_sol = sort(unique(sol_uniq))
bins_used_uniq_sol

bins_used_each_type = all_bins[bins_used_uniq_sol]
bins_used_each_type

counts = as.data.frame(table(factor(bins_used_each_type, levels = bin_types$bin_types)))
colnames(counts) = c("BinType", "Count")
print(counts)



###. mixture items lr vs rn 

content_late = list()
content_cost = list()
for(i in 1:5){
  file_name = paste0("Results_Combination29.", i, ".RData")
  load(file_name)
  
  content_late[[i]] = test$load_imbalance_eval
  content_cost[[i]] = test$cost_eval
}

for(i in 1:5){
  file_name = paste0("Results_Combination35.", i, ".RData")
  load(file_name)
  
  content_late[[i+5]] = test$load_imbalance_eval
  content_cost[[i+5]] = test$cost_eval
}


plot(content_cost[[1]], type = "l", ylim = c(1000, 14000), col="blue",
     ylab = "Mean Cost", xlab = "Generation", cex.lab = 1.5)
lines(content_cost[[2]], type = "l", col="blue")
lines(content_cost[[3]], type = "l", col="blue")
lines(content_cost[[4]], type = "l", col="blue")
lines(content_cost[[5]], type = "l", col="blue")
lines(content_cost[[6]], type = "l", col = "red")
lines(content_cost[[7]], type = "l", col = "red")
lines(content_cost[[8]], type = "l", col = "red")
lines(content_cost[[9]], type = "l", col = "red")
lines(content_cost[[10]], type = "l", col = "red")

plot(content_late[[1]], type = "l", ylim = c(0, 1500), col="blue",
     ylab = "Mean Lateness", xlab = "Generation", cex.lab = 1.5)
lines(content_late[[2]], type = "l", col="blue")
lines(content_late[[3]], type = "l", col="blue")
lines(content_late[[4]], type = "l", col="blue")
lines(content_late[[5]], type = "l", col="blue")
lines(content_late[[6]], type = "l", col = "red")
lines(content_late[[7]], type = "l", col = "red")
lines(content_late[[8]], type = "l", col = "red")
lines(content_late[[9]], type = "l", col = "red")
lines(content_late[[10]], type = "l", col = "red")




