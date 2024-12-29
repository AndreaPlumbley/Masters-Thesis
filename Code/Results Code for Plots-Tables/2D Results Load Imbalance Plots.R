# Function to extract non-dominated solutions based on cost and load imbalance
identify_non_dominated_load_imbalance <- function(cost, load_imbalance) {
  results_unique <- unique(cbind(cost, load_imbalance))
  results_unique <- cbind(1:nrow(results_unique), results_unique)
  results_unique <- as.data.frame(results_unique)
  colnames(results_unique) <- c("id", "cost", "load_imbalance")
  
  non_dominated <- rep(TRUE, nrow(results_unique))
  for (i in 1:nrow(results_unique)) {
    for (j in 1:nrow(results_unique)) {
      if (i != j && 
          results_unique$cost[j] <= results_unique$cost[i] &&
          results_unique$load_imbalance[j] <= results_unique$load_imbalance[i] &&
          (results_unique$cost[j] < results_unique$cost[i] || 
           results_unique$load_imbalance[j] < results_unique$load_imbalance[i])) {
        non_dominated[i] <- FALSE
        break
      }
    }
  }
  
  results_unique[non_dominated, ]
}


file_name = paste0("Results_Combination42.6.RData")

load(file_name)

gen500 = test$prev_generation_parents[[6]]
gen500_rotations = test$prev_generation_parents_rotations[[6]]


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

constraint_test_50 = constraint_violation_calc(gen50, gen50_rotations, numCompulsory, item_info,
                                               bin_heights_all,
                                               bin_widths_all,total_num_bins)

constraint_test_50$violation

test_eval_cost_50 = evaluate_cost(gen50, bin_cost_all, item_profits, 
                                  numCompulsory)
test_eval_cost_50


test_eval_load_imbalance_50 = evaluate_load_imbalance(gen50,gen50_rotations,
                                                      item_info, 
                                                      bin_heights_all, 
                                                      bin_widths_all,
                                                      constraint_test_50$bottom_corners)
test_eval_load_imbalance_50

## Gen 100 

constraint_test_100 = constraint_violation_calc(gen100, gen100_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_100$violation

test_eval_cost_100 = evaluate_cost(gen100, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_100


test_eval_load_imbalance_100 = evaluate_load_imbalance(gen100, gen100_rotations,
                                                       item_info, 
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_100$bottom_corners)
test_eval_load_imbalance_100


## Gen 200 

constraint_test_200 = constraint_violation_calc(gen200, gen200_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_200$violation

test_eval_cost_200 = evaluate_cost(gen200, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_200


test_eval_load_imbalance_200 = evaluate_load_imbalance(gen200, gen200_rotations,
                                                       item_info, 
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_200$bottom_corners)
test_eval_load_imbalance_200


## Gen 300 

constraint_test_300 = constraint_violation_calc(gen300, gen300_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_300$violation

test_eval_cost_300 = evaluate_cost(gen300, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_300


test_eval_load_imbalance_300 = evaluate_load_imbalance(gen300, gen300_rotations,
                                                       item_info, 
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_300$bottom_corners)
test_eval_load_imbalance_300



## Gen 400 

constraint_test_400 = constraint_violation_calc(gen400, gen400_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_400$violation

test_eval_cost_400 = evaluate_cost(gen400, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_400


test_eval_load_imbalance_400 = evaluate_load_imbalance(gen400, gen400_rotations,
                                                       item_info, 
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_400$bottom_corners)
test_eval_load_imbalance_400

## Gen 500 

constraint_test_500 = constraint_violation_calc(gen500, gen500_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_500$violation

test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_500


test_eval_load_imbalance_500 = evaluate_load_imbalance(gen500, gen500_rotations,
                                                       item_info,
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_500$bottom_corners)
test_eval_load_imbalance_500

## Gen 500 

constraint_test_500 = constraint_violation_calc(gen500, gen500_rotations, numCompulsory, item_info,
                                                bin_heights_all,
                                                bin_widths_all,total_num_bins)

constraint_test_500$violation

test_eval_cost_500 = evaluate_cost(gen500, bin_cost_all, item_profits, 
                                   numCompulsory)
test_eval_cost_500


test_eval_load_imbalance_500 = evaluate_load_imbalance(gen500, gen500_rotations,
                                                       item_info,
                                                       bin_heights_all, 
                                                       bin_widths_all,
                                                       constraint_test_500$bottom_corners)
test_eval_load_imbalance_500


plot(test_eval_cost_50~test_eval_load_imbalance_50, pch = 16, cex = 1.2, col = "red", 
     xlim=c(700,3000), ylim = c(0,15000), xlab = "Load Imbalance",
     ylab = "Cost Objective", cex.lab = 1.5)
points(test_eval_cost_100~test_eval_load_imbalance_100, pch = 16,cex = 1.2, col = "blue")
points(test_eval_cost_200~test_eval_load_imbalance_200, pch = 16,cex = 1.2, col = "orange")
points(test_eval_cost_300~test_eval_load_imbalance_300, pch = 16,cex = 1.2, col = "brown")
points(test_eval_cost_400~test_eval_load_imbalance_400, pch = 16, cex = 1.2,col = "darkgreen")
points(test_eval_cost_500~test_eval_load_imbalance_500, pch = 16, cex = 1.2,col = "green")
legend("topleft", legend=c("50 generations", "100 generations", "200 generations", "300 generations", "400 generations", "500 generations"), 
       col=c("red", "blue", "orange","brown", "darkgreen","green"), pch=16, pt.cex = 1.5)




non_dom_500 = identify_non_dominated_load_imbalance(test_eval_cost_500, test_eval_load_imbalance_500)
print(non_dom_500)

plot(non_dom_500$cost~non_dom_500$load_imbalance, col = "darkgreen", pch = 16,
     xlab = "Load Imbalance", ylab = "Cost", cex.lab = 1.5, cex = 1.2)


library(ggplot2)

get_packing_sols = function(parents, parent_rotations, bottom_corners, 
                            item_info, bin_heights, bin_widths){
  
  uniq_sols = unique(parents[,-1])
  uniq_sol = uniq_sols[2,]
  
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
    colnames(item_info_j) = c("item_id", "height", "width","weight", "x", "y")
    
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

test_plots = get_packing_sols(parents = gen500, parent_rotations = gen500_rotations, 
                              bottom_corners = constraint_test_500$bottom_corners, 
                              item_info = item_info, 
                              bin_heights = bin_heights_all, 
                              bin_widths = bin_widths_all)
test_plots
library(gridExtra)

# Assuming your plots are stored in p1, p2, ..., p9
grid.arrange(test_plots[[1]], test_plots[[2]], test_plots[[3]], 
             test_plots[[4]], test_plots[[5]], test_plots[[6]], 
             test_plots[[7]], test_plots[[8]], test_plots[[9]], 
             ncol = 3)



uniq_sol = unique(gen500[,-1])
uniq_sol


evaluate_cost(cbind(1:nrow(uniq_sol),uniq_sol), bin_cost_all, item_profits, numCompulsory)
non_dom_500

## CONVERGENCE PLOTS

###. mixture items lr vs rn 

content_load = list()
content_cost = list()
for(i in 6:10){
  file_name = paste0("Results_Combination27.", i, ".RData")
  load(file_name)
  
  content_load[[i-5]] = test$load_imbalance_eval
  content_cost[[i-5]] = test$cost_eval
}

for(i in 6:10){
  file_name = paste0("Results_Combination33.", i, ".RData")
  load(file_name)
  
  content_load[[i]] = test$load_imbalance_eval
  content_cost[[i]] = test$cost_eval
}


plot(content_cost[[1]], type = "l", col="blue", ylim = c(0, 14000),
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

plot(content_load[[1]], type = "l", ylim = c(300, 3000), col="blue",
     ylab = "Mean Load Imbalance", xlab = "Generation", cex.lab = 1.5)
lines(content_load[[2]], type = "l", col="blue")
lines(content_load[[3]], type = "l", col="blue")
lines(content_load[[4]], type = "l", col="blue")
lines(content_load[[5]], type = "l", col="blue")
lines(content_load[[6]], type = "l", col = "red")
lines(content_load[[7]], type = "l", col = "red")
lines(content_load[[8]], type = "l", col = "red")
lines(content_load[[9]], type = "l", col = "red")
lines(content_load[[10]], type = "l", col = "red")


