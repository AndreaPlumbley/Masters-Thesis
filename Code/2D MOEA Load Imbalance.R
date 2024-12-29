## 2D MOEA for cost and load imbalance objectives
## Andrea Plumbley

## =============== CONSTURCTIVE HEURISTIC =====================================
# scoring_items is a function that takes a segment in a skyline along with items
# that fit in that segment and scores them according the the scoring table
scoring_items = function(items_height_width, segment){
  # items_height_width: first column being item number, second being height, 
  # third being width
  
  # segment is a vector with its five characteristics, bottom left x coordinate, 
  # bottom left y coordinate, width, h1 and h2. 
  
  num_items = nrow(items_height_width)
  if(is.null(num_items)){
    num_items = 1
  }
  
  sort_items_height_width = items_height_width[order(items_height_width[,2],  
                                                     items_height_width[,3], 
                                                     decreasing = TRUE),]
  
  w = segment$width
  h1 = segment$h1
  h2 = segment$h2
  
  scores = c()
  
  if(h1>=h2){
    for(i in 1:num_items){
      if(w == sort_items_height_width[i,3] & h1 == sort_items_height_width[i,2]){
        score = 4
      } else if (w == sort_items_height_width[i,3] & h1 < sort_items_height_width[i,2]){
        score = 3
      } else if (w == sort_items_height_width[i,3] & h1 > sort_items_height_width[i,2]){
        score = 2
      } else if (w > sort_items_height_width[i,3] & h1 == sort_items_height_width[i,2]){
        score = 1
      } else {
        score = 0
      }
      
      scores[i] = score
      
    }
  } else if(h1 < h2){
    for(i in 1:num_items){
      if(w == sort_items_height_width[i,3] & h2 == sort_items_height_width[i,2]){
        score = 4
      } else if (w == sort_items_height_width[i,3] & h2 < sort_items_height_width[i,2]){
        score = 3
      } else if (w == sort_items_height_width[i,3] & h2 > sort_items_height_width[i,2]){
        score = 2
      } else if (w > sort_items_height_width[i,3] & h2 == sort_items_height_width[i,2]){
        score = 1
      } else {
        score = 0
      }
      
      scores[i] = score
      
    }
  }
  return(cbind(sort_items_height_width[,1],scores))
}

## Update skyline function, for when a new item is packed into a bin
update_skyline = function(skyline, item_x_coord, item_y_coord, item_width, 
                          item_height) {
  new_skyline = list()
  item_right_x = item_x_coord + item_width
  item_top_y = item_y_coord + item_height
  
  i = 1
  while (i <= length(skyline)) {
    segment = skyline[[i]]
    
    # Case 1: The segment is entirely to the left of the item
    if (segment$x + segment$width < item_x_coord) {
      new_skyline = append(new_skyline, list(segment))
      
      # Case 2: The segment is entirely to the right of the item
    } else if (segment$x > item_right_x) {
      new_skyline = append(new_skyline, list(segment))
      
      
    } else if (segment$x + segment$width == item_x_coord){
      #Update h2
      new_skyline = append(new_skyline, list(
        list(x = segment$x, y = segment$y, width = segment$width, 
             h1 = segment$h1, h2 = max(0,item_top_y-segment$y))
      ))
    } else if (segment$x == item_right_x){
      #Update h2
      new_skyline = append(new_skyline, list(
        list(x = segment$x, y = segment$y, width = segment$width, 
             h1 = max(0,item_top_y-segment$y), h2 = segment$h2)
      ))
    }else {
      # Case 3: The segment overlaps with the item (partial or full)
      # First case: item is packed on left of segment
      if(segment$x == item_x_coord){
        if(segment$width == item_width){ #Then item covers whole segment
          new_skyline = append(new_skyline, list(
            list(x = segment$x, y = segment$y + item_height, width = item_width, 
                 h1 = max(0,segment$h1 - item_height), 
                 h2 = max(0,segment$h2 - item_height))
          ))
        }else{ #Create segment for item packed on left and remaining segment
          new_skyline = append(new_skyline, list(
            list(x = item_x_coord, y = item_y_coord + item_height, 
                 width = item_width, h1 = max(segment$h1 - item_height,0), 
                 h2 = 0)
          ))
          new_skyline = append(new_skyline, list(
            list(x = segment$x + item_width, y = segment$y, 
                 width = segment$width - item_width, h1 = item_height, 
                 h2 = segment$h2)
          ))
        }
      } else if (segment$x < item_x_coord){     # Second case: item is packed on right of segment
        new_skyline = append(new_skyline, list(
          list(x = segment$x, y = segment$y, width = segment$width - item_width, 
               h1 = segment$h1, h2 = item_height)
        ))
        new_skyline = append(new_skyline, list(
          list(x = item_x_coord, y = segment$y+item_height, width = item_width, h1 = 0, 
               h2 = max(0,segment$h2 - item_height))
        ))
      }
    }
    i = i + 1
  }
  
  # Merge adjacent skyline segments with the same height
  merged_skyline = list()
  prev_segment = new_skyline[[1]]
  
  if(length(new_skyline)>1){
    for (i in 2:length(new_skyline)) {
      curr_segment = new_skyline[[i]]
      
      if (prev_segment$y == curr_segment$y) {
        prev_segment$width = prev_segment$width + curr_segment$width
        prev_segment$h2 = curr_segment$h2
      } else {
        merged_skyline = append(merged_skyline, list(prev_segment))
        prev_segment = curr_segment
      }
    }
    merged_skyline = append(merged_skyline, list(prev_segment))
  }else{
    merged_skyline = new_skyline
  }
  
  return(merged_skyline)
}

## Find lowest and leftmost segment
find_lowest_left = function(skyline){
  lowest_left_seg = skyline[[1]]
  
  for(segment in skyline){
    if(segment$y<lowest_left_seg$y){
      lowest_left_seg = segment
    } else if (segment$y == lowest_left_seg$y && segment$x < lowest_left_seg$x){
      lowest_left_seg = segment
    }
  }
  return(lowest_left_seg)
}

## Constructive Heuristic 

## Input: A list of P items to be packed with dimensions width and height, and a strip width W. 
## In our case the strip width is the width of our 2D bin. 

# Output: A feasible packing of the items in P into a strip of width W, and the packing
# height h. Just generally feasible or not? and if feasible then the arrangement. 

# Step 1: Initialize the skyline 

initial_skyline = function(bin_width, bin_height){
  skyline = list(list(x = 0, y = 0, width = bin_width, h1 = bin_height, h2 = bin_height))
  return(skyline)
}

constructive_heuristic = function(bin_width, bin_height, item_info){
  
  num_items = nrow(item_info)
  
  h = 0
  num_packed_items = 0
  skyline = initial_skyline(bin_width, bin_height)
  
  corner_points = matrix(NA, nrow = num_items, ncol = 3)
  corner_points[,1] = item_info[,1]
  
  while(num_packed_items<num_items){
    if(h>=bin_height){
      return(list(h=h, num_packed_items = num_packed_items, 
                  corner_points = corner_points))
    }else{
      ## Find lowest and left most skyline segment - just lowest x and y coords
      ## Call this segment s
      lowest_left_segment = find_lowest_left(skyline)
      
      items_that_can_fit = item_info[item_info[,3]<=lowest_left_segment$width,]
      
      if(nrow(items_that_can_fit)>=1){
        ## If there is an item that could fit into S then
        
        ##for each unpacked item i {
        ## (Assuming this is for items that could actually fit. )
        ##Get item score using scoring_items() }
        scores = scoring_items(items_that_can_fit, lowest_left_segment)
        
        # Select item R with max score
        max_score_index = which.max(scores[,2])
        item_number = scores[max_score_index,1]
        item_height = items_that_can_fit[items_that_can_fit[,1]==item_number,2]
        item_width = items_that_can_fit[items_that_can_fit[,1]==item_number,3]
        
        if(lowest_left_segment$y + item_height > h){
          h = lowest_left_segment$y + item_height
        }
        
        if(h>bin_height){
          return(list(h=h, num_packed_items = num_packed_items, 
                      corner_points = corner_points))
        }
        
        if(lowest_left_segment$h1 >= lowest_left_segment$h2){
          corner_points[corner_points[,1]==item_number, 2:3] = c(lowest_left_segment$x, lowest_left_segment$y)
          skyline = update_skyline(skyline = skyline, item_x_coord = lowest_left_segment$x,
                                   item_y_coord = lowest_left_segment$y, item_width = item_width,
                                   item_height = item_height)
          num_packed_items = num_packed_items + 1
          item_info = item_info[item_info[,1]!=item_number,]
        }else{
          corner_points[corner_points[,1]==item_number, 2:3] = c(lowest_left_segment$x + lowest_left_segment$width - item_width,
                                                                 lowest_left_segment$y)
          skyline = update_skyline(skyline = skyline, 
                                   item_x_coord = lowest_left_segment$x + lowest_left_segment$width - item_width,
                                   item_y_coord = lowest_left_segment$y, item_width = item_width,
                                   item_height = item_height)
          num_packed_items = num_packed_items + 1
          item_info = item_info[item_info[,1]!=item_number,]
        }
      }else{
        lowest_neighbour = min(lowest_left_segment$h1, lowest_left_segment$h2)
        skyline = update_skyline(skyline, lowest_left_segment$x, lowest_left_segment$y,
                                 lowest_left_segment$width,
                                 lowest_neighbour)
        #print(lowest_neighbour)
        #print(h)
        if((lowest_left_segment$y + lowest_neighbour) > h){
          h = lowest_left_segment$y + lowest_neighbour
        }
      }
    }
  }
  return(list = list(h=h, corner_points = corner_points, num_packed_items = num_packed_items))
}

## ========================================================================= ##

## MOEA ALGORITHM FUNCTIONS:

## Load Imbalance

calculate_load_imbalance = function(item_info_WCGB, desired_bin_CGs, binsUsed){
  
  item_weights = item_info_WCGB[,2]
  item_CG_x = item_info_WCGB[,3]
  item_CG_y = item_info_WCGB[,4]
  item_bin = item_info_WCGB[,5]
  
  if(is.null(nrow(desired_bin_CGs))){
    desired_bin_CG_x = desired_bin_CGs[2]
    desired_bin_CG_y = desired_bin_CGs[3]
  }else{
    desired_bin_CG_x = desired_bin_CGs[,2]
    desired_bin_CG_y = desired_bin_CGs[,3]
  }
  
  item_weight_times_CGx = item_weights*item_CG_x
  item_weight_times_CGy = item_weights*item_CG_y
  
  imbalance_ind_bins = c()
  
  for(i in binsUsed){
    items_in_bin = which(item_bin==i)
    bin_cg_x = sum(item_weight_times_CGx[items_in_bin])/sum(item_weights[items_in_bin])
    bin_cg_y = sum(item_weight_times_CGy[items_in_bin])/sum(item_weights[items_in_bin])
    
    imbalance_ind_bins[i] = sqrt((bin_cg_x-desired_bin_CG_x[i])^2+(bin_cg_y-desired_bin_CG_y[i])^2)
  }
  
  total_imbalance = sum(na.omit(imbalance_ind_bins))
  return(total_imbalance)
}


item_centre_gravity = function(bottom_left_coordinates, top_right_coordinates){
  
  bottom_x = as.numeric(bottom_left_coordinates[,1])
  bottom_y = as.numeric(bottom_left_coordinates[,2])
  
  top_x = as.numeric(top_right_coordinates[,1])
  top_y = as.numeric(top_right_coordinates[,2])
  
  up_down_half = (top_y - bottom_y)/2
  side_side_half = (top_x - bottom_x)/2
  
  centre_gravity = cbind(bottom_x + side_side_half, bottom_y+ up_down_half)
  return(centre_gravity)  
}


get_top_coords = function(item_heights, items_widths, bottom_corners){
  
  item_number = bottom_corners[,2]
  
  bottom_corner_x = bottom_corners[,3]
  bottom_corner_y = bottom_corners[,4]
  
  top_right_x = c()
  top_right_y = c()
  
  for(i in 1:length(item_number)){
    top_right_x[i] = bottom_corner_x[[i]] + item_widths[item_number[[i]]]
    top_right_y[i] = bottom_corner_y[[i]] + item_heights[item_number[[i]]]
  }
  return(cbind(top_right_x, top_right_y))
}

## The fast_non_dom_sort() function has been written, trying to be as general
## as possible so that many different problems can be applied. 

## Within the function, a check needs to be done of whether one solution 
## dominates another solution. A function, dominates(), is written to 
## check this.

## A solution dominates another solutions if it is better in at least one 
## objective and the same as or better in the other objectives. Because we are
## considering minimization, a solution dominates another if it has smaller
## values for the objectives. 

## The function dominates takes in 3 parameters, p, q and P. P is a matrix 
## with its first column being solution indices. The remaining columns contain
## the objective function values of each solution. p and q the indices of the 
## two solutions that we wish to compare. Specifically we want to know if 
## solution p dominates solution q and this function returns a boolean value. 


dominates = function(p, q, P){
  num_objectives = dim(P)[2] - 1   # Calculate the number of objectives
  p_obj_values = P[p,-1]           # Extract the obj vals for sol p
  q_obj_values = P[q,-1]           # Extract the obj vals for sol q
  
  # Initially set the boolean to FALSE
  p_dominates_q = FALSE
  
  # For each objective compare the obj vals of p and q
  # If p has any obj vals > than obj vals of q, p cannot dominate q and FALSE
  # is returned by the function immediately. 
  
  for (i in 1:num_objectives) {
    if (p_obj_values[i] > q_obj_values[i]) {
      return(FALSE)                       
    } else if (p_obj_values[i] < q_obj_values[i]) {
      p_dominates_q = TRUE
    }
  }
  return(p_dominates_q)
}


dominates_with_constrained_dom = function(p, q, P){
  num_col = dim(P)[2]
  num_objectives = num_col - 2   # Calculate the number of objectives
  p_obj_values = P[p, -c(1, num_col)] # Extract the obj vals for sol p
  q_obj_values = P[q, -c(1, num_col)] # Extract the obj vals for sol q
  
  p_constraint_val = P[p, num_col]
  q_constraint_val = P[q, num_col]
  
  # Initially set the boolean to FALSE
  p_dominates_q = FALSE
  
  if(p_constraint_val>q_constraint_val){
    return(FALSE)
  } else if(p_constraint_val<q_constraint_val){
    return(TRUE)
  }else{
    # For each objective compare the obj vals of p and q
    # If p has any obj vals > than obj vals of q, p cannot dominate q and FALSE
    # is returned by the function immediately. 
    for (i in 1:num_objectives) {
      if (p_obj_values[i] > q_obj_values[i]) {
        return(FALSE)                       
      } else if (p_obj_values[i] < q_obj_values[i]) {
        p_dominates_q = TRUE
      }
    }
  }  
  return(p_dominates_q)
}

fast_non_dom_sort = function(P){
  # P must be a matrix containing solutions' objective function values.  
  # P does not contain the actual solution gene representation (for GBPP) or 
  # solution parameters for a general problem. 
  # The first column of P contains an index of the solutions and the remaining
  # columns are the objective function values for that solution for the m
  # objectives. Number of objectives = m = num columns in P - 1
  
  num_solutions = dim(P)[1]         # Get the number of solutions
  num_objectives = dim(P)[2] - 1    # Get the number of objectives
  
  rank = rep(NA, num_solutions)     #Space to store rank of each solution
  
  #Store the sets of solutions dominated by p in a list
  # S[[p]] gives set of solutions dominated by p
  S = list()                       
  
  F_1 = c()     #F_1 is empty first non-dominated front
  
  non_dom_count = c()    #Vector to store non-domination counts for each sol
  
  
  # First for loop calculates a solutions n_p and S_p
  
  for(p in 1:num_solutions){
    S_p = c() #Set S_p to initially be empty
    n_p = 0   #Set initial domination count to 0 
    for(q in 1:num_solutions){ #Inner loop to compare all sols with each other
      if(dominates_with_constrained_dom(p, q, P)){
        S_p = c(S_p, q)   #Add solution q to the set S_p if p dominates q
      }else if (dominates_with_constrained_dom(q, p, P)){
        n_p = n_p + 1   #Increment the domination count of p if q dominates p
      }
    }
    # If domination count is zero then solution is in the first front
    # For all solutions where n_p=0, assign sol a rank of 1 and add it to F1
    if(n_p == 0){       
      rank[p] = 1
      F_1 = c(F_1, p)
    }
    # Add the domination count and set of sols p dominates to the stored vector
    # non_dom_count and list S. If S_p remained empty assign 0 to S[[p]]
    non_dom_count = c(non_dom_count, n_p)
    if(is.null(S_p)){ 
      S[[p]] = 0
    } else {
      S[[p]] = S_p
    }
  }
  
  # Create a list, Fronts, which stored the solutions in each non-dominated
  # front. Eg Fronts[[1]] contains vector of solutions in first front. 
  Fronts = list()
  Fronts[[1]] = F_1
  
  # The second loop is a while loop with a boolean variable that checks if 
  # there are still remaining solutions in the next front. The boolean indicates
  # if we have assigned all solutions to a front by giving them a rank. 
  
  # A initial check must first be done whether or not all solutions were in 
  # the first front. 
  
  if(length(F_1) == dim(P)[1]){
    next_front_not_empty = FALSE
  }else{
    next_front_not_empty = TRUE
  }
  
  i = 1    #Initialize the front counter
  while(next_front_not_empty){
    Q = c()   #Create empty set which will contain solutions in the next front
    # For each solution in the current front, get the solutions that it 
    # dominates and check whether they are in the next front.
    for(p in Fronts[[i]]){ 
      # If S[[p]] is empty, then solution p dominates no solutions and we do 
      # not need to iterate through the inner loop. So a constraint is added to
      # check if there are any solutions in S[[p]].
      if(sum(S[[p]])>0){
        for(q in S[[p]]){
          # Decrement domination count of each sol q in S[[p]]
          non_dom_count[q] = non_dom_count[q] - 1
          if(non_dom_count[q]==0){ #If true then q belongs to next front
            rank[q] = i + 1
            Q = c(Q, q)
          }
        }
      }
    }
    i = i + 1
    Fronts[[i]] = Q
    
    # Check whether there are still solutions to be assingned 
    # This can be done by seeing if any non-domination counts are not yet
    # equal to zero. If all non-domination counts are equal to zero, it means
    # that all solutions have been assigned a rank and to a front. 
    if(sum(non_dom_count)==0){ 
      next_front_not_empty = FALSE
    }
  }
  return(list = list(Rank = rank, Fronts = Fronts))
}


crowding_dist_assign = function(I){
  # I needs to be a matrix, similar format to P, where the first column 
  # contains an index of the solutions and the remaining columns contain the 
  # objective function values for each of the m objectives.
  
  if(is.null(nrow(I))){
    return(c(I[1], Inf))
  }else{
    
    l = dim(I)[1]                     # Number of solutions in I
    num_objectives = dim(I)[2] - 1    # Number of objectives 
    
    #Add a column to matrix I to contained crowding distances for each solution
    I_sorted = cbind(I, rep(0,l))
    
    # Get the column number of the column containing the crowding distances
    # for indexing purposes. 
    dist_col = num_objectives+2
    
    for(m in 1:num_objectives){
      
      # Calculate max and min objective function values
      f_m_max = max(I_sorted[,m+1])
      f_m_min = min(I_sorted[,m+1])
      
      # Sort the solutions based on objective m
      order_index = order(I_sorted[,m+1])
      I_sorted = I_sorted[order_index,]   
      
      # Set a distance value of Inf for solutions with max and min for obj m
      I_sorted[1,dist_col] = Inf
      I_sorted[l,dist_col] = Inf
      
      # The check for more than 2 solutions is to avoid indexing problems in
      # the for loop. 
      if(l > 2 & (f_m_max-f_m_min>0)){
        for(i in 2:(l-1)){
          I_sorted[i, dist_col] = I_sorted[i, dist_col] + 
            (I_sorted[i+1,m+1]-I_sorted[i-1,m+1])/(f_m_max-f_m_min)
        }
      } else{
        for(i in 2:(l-1)){
          I_sorted[i, dist_col] = Inf
        }
      }
    }
    
    # Re order I so that solutions are in original index order
    order_index_normal = order(I_sorted[,1])
    I_sorted = I_sorted[order_index_normal,]
    return(I_sorted[,c(1,dist_col)])
  }
}


## crowd_comp_operator() returns the solution that is preferred between i and j
crowd_comp_operator = function(i, j){
  # i and j must be vectors with three components each: solution index, rank 
  # and crowding distance.
  
  i_rank = i[2]    # Get rank of solution i
  i_crowd = i[3]   # Get crowding distance of solution i 
  
  j_rank = j[2]    # Get rank of solution j
  j_crowd = j[3]   # Get crowding distance of solution j
  
  # Determine preferred solution
  if(i_rank < j_rank){
    solution_preferred = i  
  }else if((i_rank==j_rank) & (i_crowd>j_crowd)){
    solution_preferred = i    
  }else{
    solution_preferred = j
  }
  return(solution_preferred)
}

## sort_crowd_comp() takes a set of solutions and sorts them according to the
## crowd comparison operator. The function returns the sorted set.
sort_crowd_comp = function(solSet){
  # solSet is a set of solutions and should be a matrix, with the first column
  # containing solution indices, the second column containing the rank values 
  # of these solutions and the third column containing the crowding distance
  # values for each of the solutions. 
  
  # Order the solution set by rank first and then by crowding distance 
  sorted_by_rank = order(solSet[,2], -solSet[,3]) 
  
  return(solSet[sorted_by_rank,])
}


evaluate_cost = function(solution_pop, bin_costs, item_profits, numCompulsory){
  
  N = nrow(solution_pop)
  evals = rep(0, N)
  total_items = length(item_profits)
  
  #Remove index column
  solution_pop = solution_pop[,-1]
  
  for(i in 1:N){
    bins_used = sort(unique(solution_pop[i,]))
    bin_total_cost = sum(bin_costs[bins_used])
    
    if(numCompulsory == num_items){
      prof_unloaded_nc_items = 0
    }else{
      nc_items = solution_pop[i, (numCompulsory+1):total_items]
      nc_items_profits = item_profits[(numCompulsory+1):total_items]
      
      nc_items_not_loaded = which(nc_items==0)
      prof_unloaded_nc_items = sum(nc_items_profits[nc_items_not_loaded])
    }
    evals[i] = bin_total_cost + prof_unloaded_nc_items
  }
  return(evals)
}



evaluate_load_imbalance = function(solution_pop, solution_pop_rotations, item_info, 
                                   bin_height, bin_width, bottom_corners){
  
  # Item info is a matrix, first column index, second column height, third
  # column widths, last column weights
  
  N = nrow(solution_pop)
  n_items = nrow(item_info)
  imbalances = c()
  desired_CGs = cbind(c(1:length(bin_height)),rep(0, length(bin_height)), bin_width/2)
  
  #Remove index column
  solution_pop = solution_pop[,-1]
  solution_pop_rotations = solution_pop_rotations[,-1]
  
  for(i in 1:N){
    this_item_info = update_info_rotate(item_info, solution_pop_rotations[i,])
    this_bottom_corners = bottom_corners[[i]]
    
    if(length(this_bottom_corners)==0){ # this only happens if no items fit in any bins 
      imbalances[i] = 5000
    }else{
      
      items_in = as.numeric(this_bottom_corners[,1])
      
      bottom_x =  as.numeric(this_bottom_corners[,2])
      bottom_y =  as.numeric(this_bottom_corners[,3])
      top_corners = cbind(bottom_x + this_item_info[items_in,3], bottom_y + this_item_info[items_in,2])
      
      if(length(items_in)==1){
        bottom_corners_coords = cbind(this_bottom_corners[,c(2,3)][[1]],
                                      this_bottom_corners[,c(2,3)][[2]])
      }else{
        bottom_corners_coords = this_bottom_corners[,c(2,3)]
      }
      
      item_cg = item_centre_gravity(bottom_corners_coords, top_corners)
      
      bins_used = unique(as.numeric(this_bottom_corners[,4]))
      num_bins_fitting = length(bins_used)
      
      item_info_WCGB = cbind(items_in, this_item_info[items_in,4], 
                             item_cg, as.numeric(this_bottom_corners[,4]))
      
      
      imbalances[i] = calculate_load_imbalance(item_info_WCGB, desired_CGs, bins_used)
    }
  }
  return(imbalances)
}

constraint_violation_calc = function(solution_pop, solution_pop_rotations,
                                     numCompulsory, item_info, 
                                     bin_heights,bin_widths, totalNumBins){
  N = nrow(solution_pop)
  violation_score = numeric(N)  # Create space for violation scores
  
  #Remove index column
  solution_pop = solution_pop[,-1]
  solution_pop_rotations = solution_pop_rotations[,-1]
  bottom_corners = vector("list", N)
  
  for (i in 1:N){

    item_info_this_sol_update = update_info_rotate(item_info, solution_pop_rotations[i,])
    
    # First constraint - check that all compulsory items are packed
    #items_packed = ifelse(solution_pop[i,]>0,1,0)
    items_packed = as.integer(solution_pop[i,] > 0)
    c_items_not_packed = numCompulsory - sum(items_packed[1:numCompulsory])
    
    # Second constraint - check the weight are not more than capacity
    weight_violation = numeric(totalNumBins)
    bottom_corners[[i]] = list()
    
    unique_bins = unique(solution_pop[i,])
    
    for(j in unique_bins){
      if(j > 0){
        #Check which items are in bin j
        items_in_bin_j = which(solution_pop[i,]==j) 
        
        #Get the item info
        item_info_j = item_info_this_sol_update[items_in_bin_j,]
        
        if(is.null(nrow(item_info_j))){
          item_info_j_data_frame = as.data.frame(t(item_info_j))
        }else{
          item_info_j_data_frame = as.data.frame(item_info_j)
        }
        
        item_placement = constructive_heuristic(bin_widths[j], bin_heights[j],
                                                item_info_j_data_frame)
        
        
        if(item_placement$num_packed_items<length(items_in_bin_j)){
          #fit_boolean = FALSE
          weight_violation[j] = 100 * (length(items_in_bin_j) - item_placement$num_packed_items)
        } else{
          #fit_boolean = TRUE
          b_cs_with_bin = cbind(item_placement$corner_points, rep(j, item_placement$num_packed_items))
          bottom_corners[[i]] = rbind(bottom_corners[[i]], b_cs_with_bin)
        }
      }
    }
    violation_score[i] = 50*c_items_not_packed + sum(weight_violation)
  }
  return(list = list(violation = violation_score, bottom_corners = bottom_corners))
}


initial_pop = function(totalNumItems, totalNumBins, population_size){
  pop_init = matrix(sample(0:totalNumBins,totalNumItems*population_size,replace=TRUE),
                    nrow=population_size)
  
  pop_initial = cbind(c(1:population_size), pop_init)
  
  return(list = list(initial_pop_sol = pop_initial))
}

initial_pop_rotations = function(totalNumItems, population_size) {
  # Initialize the population matrix with random -1 (no rotation) and 1 (rotation)
  pop_init = matrix(sample(c(-1, 1), totalNumItems * population_size, replace = TRUE), 
                    nrow = population_size, ncol = totalNumItems)
  
  # Combine the population IDs with the population matrix
  pop_initial = cbind(c(1:population_size), pop_init)
  
  # Return the initial population
  return(list(initial_rotation = pop_initial))
}


# Binary tournament selection
# This function requires a population of solutions, the ranks of these solutions
# and the constraint violation values for this solutions. 

selection_binary_tourn = function(solution_population, solution_pop_rotation,
                                  ranks, constraint_violations, crowd_distance){
  
  # Remember that the solution_population has a first column with solution indices
  N = dim(solution_population)[1]
  
  selected_parents = matrix(NA, N, dim(solution_population)[2])
  selected_rotation = matrix(NA, N, dim(solution_population)[2])
  
  for(i in 1:N){
    
    candidates = sample(1:N, 2)
    ind1 = candidates[1]
    ind2 = candidates[2]

    # First check feasibility. 
    if(constraint_violations[ind1] < constraint_violations[ind2]){
      better_sol = ind1
    } else if (constraint_violations[ind2] < constraint_violations[ind1]){
      better_sol = ind2
    } else if(constraint_violations[ind1]==0){
      if(ranks[ind1]<ranks[ind2]){
        better_sol = ind1
      }else if (ranks[ind2]<ranks[ind1]){
        better_sol = ind2
      }else if(crowd_distance[ind1]>crowd_distance[ind2]){
        better_sol = ind1
      }else{
        better_sol = ind2   
      }
    }else{ #This assumes both are infeasible and violation count is equal, choose one randomly
      better_sol = sample(candidates, 1)
    }
    selected_parents[i,] = solution_population[better_sol,]
    selected_rotation[i,] = solution_pop_rotation[better_sol,]
  }
  return(list = list(selected_parents = selected_parents, 
                     selected_rotation = selected_rotation))
}

# Uniform Crossover
# This function takes in the parents selected using binary tournament selection
# and returns offspring that are created using parent pairs and uniform crossover.

# Can make some parts of this be done in parallel which would make it quicker

# Parents will have a solution index that should be removed at this point 
crossover_uniform = function(parents, parents_rotation){
  
  #Remove index column
  parents = parents[,-1]
  parents_rotation = parents_rotation[,-1]
  
  population_size = dim(parents)[1]
  num_variables = dim(parents)[2]
  
  # Pick parents to mate
  parent_pairs = matrix(sample(1:population_size), population_size/2, 2)
  
  # Initialize offspring
  offsprings = matrix(NA, population_size, num_variables)
  offsprings_rotations = matrix(NA, population_size, num_variables)
  
  #Perform crossover
  for(i in 1:dim(parent_pairs)[1]){
    # Get parents
    p1 = parents[parent_pairs[i,][1], ]
    p2 = parents[parent_pairs[i,][2], ]
    
    #Get parents rotate
    p1_rotate = parents_rotation[parent_pairs[i,][1], ]
    p2_rotate = parents_rotation[parent_pairs[i,][2], ]
    
    # Make children
    c1 = rep(NA, num_variables)
    c2 = rep(NA, num_variables)
    
    c1_rotations = rep(NA, num_variables)
    c2_rotations = rep(NA, num_variables)
    
    # Apply uniform crossover to get children
    for(j in 1:num_variables){
      if(runif(1) <= 0.5){
        c1[j] = p1[j]
        c2[j] = p2[j]
        
        c1_rotations[j] = p1_rotate[j]
        c2_rotations[j] = p2_rotate[j]
        
      }else{
        c2[j] = p1[j]
        c1[j] = p2[j]
        
        c2_rotations[j] = p1_rotate[j]
        c1_rotations[j] = p2_rotate[j]
      }
    }
    # Store children
    offsprings[2*i-1, ] = c1
    offsprings[2*i, ]   = c2
    
    # Store children rotations
    offsprings_rotations[2*i-1, ] = c1_rotations
    offsprings_rotations[2*i, ]   = c2_rotations
    
  }
  return(list(offspring = offsprings, offsprings_rotations = offsprings_rotations))  
}

# Mutation 
# This is done to encourage diversity in solution pop
# Mutation function takes in the offspring generated from crossover and returns
# mutated offspring

# When offspring are returned from crossover they do not have an index column
# as the parents did 

# Swap two items in terms of bins in 
swap_mutate = function(single_sol){
  
  num_items = length(single_sol)
  swap_items = sample(1:num_items, 2, FALSE)
  
  temp_store = single_sol[swap_items[1]]
  
  single_sol[swap_items[1]] = single_sol[swap_items[2]]
  single_sol[swap_items[2]] = temp_store
  
  return(list = list(solution = single_sol))
}

# Move items to new bins a random number of them
move_mutate_any_bin = function(single_sol, totalNumBins){
  
  num_items = length(single_sol)
  num_items_to_move = sample(1:num_items,1)
  items_to_move = sample(1:num_items, num_items_to_move)
  
  new_bins = sample(0:totalNumBins, num_items_to_move, replace = TRUE)
  single_sol[items_to_move] = new_bins
  
  return(list = list(solution = single_sol))
}


merge_two_bins = function(single_sol){
  if(length(unique(single_sol))>1){
    two_bins = sample(unique(single_sol), 2, replace = FALSE)
    one_bin = sample(two_bins, 1)
    other_bin = setdiff(two_bins, one_bin)
    single_sol[which(single_sol == other_bin)] = one_bin
    return(list = list(solution = single_sol))
  }else{
    return(list = list(solution = single_sol))
  }
}

split_one_bin = function(single_sol, totalNumBins){
  unique_bins = unique(single_sol)
  
  bins_wih_multi_items = c()
  
  # Find a bin with more than one item
  for(bin in unique_bins){
    items_in_bin = which(single_sol == bin)
    if(length(items_in_bin) > 1){
      bins_wih_multi_items = c(bins_wih_multi_items, bin)
    }
  }
  
  if(length(bins_wih_multi_items)==0){
    return(list = list(solution = single_sol))
  }else{
    bin_to_split = sample(bins_wih_multi_items, 1)
    
    new_bin = sample(0:totalNumBins, 1)
    
    items_in_split_bin = which(single_sol == bin_to_split)
    
    # Randomly select half (or roughly half) of the items to move to the new bin
    split_indices = sample(items_in_split_bin, length(items_in_split_bin) / 2, replace = FALSE)
    
    # Assign the new bin to the selected items
    single_sol[split_indices] = new_bin
  }
  # If no bin has more than one item, return the original solution
  return(list = list(solution = single_sol))
}

## Rotations
rotate_items = function(rotations){
  
  num_items = length(rotations)
  
  num_items_to_rotate = sample(1:round(num_items/2), 1)
  items_to_rotate = sample(1:num_items, num_items_to_rotate)
  
  rotations_new = rotations
  rotations_new[items_to_rotate] = rotations[items_to_rotate]*-1
  
  return(rotations_new)
}


## Make overall mutation 

mutation_swap_move = function(offspring, totalNumBins, rotations,
                              mutation_prob, swap_prob){ 
  
  offspring_mutate = matrix(NA, nrow = nrow(offspring), ncol = ncol(offspring))
  rotation_offspring =  matrix(NA, nrow = nrow(rotations), ncol = ncol(rotations))
  
  for(i in 1:nrow(offspring)){
    rn1 = runif(1,0,1)
    if(rn1<0.3){
      rn = runif(1,0,1)
      
      if(rn<=0.2){
        mutation =swap_mutate(offspring[i,])
        offspring_mutate[i,] = mutation$solution
      }else if(rn<=0.5){
        mutation = merge_two_bins(offspring[i,])
        offspring_mutate[i,] = mutation$solution
      }else if(rn<=0.8){
        mutation = split_one_bin(offspring[i,], totalNumBins)
        offspring_mutate[i,] = mutation$solution
      }else{
        mutation = move_mutate_any_bin(offspring[i,], totalNumBins)
        offspring_mutate[i,] = mutation$solution
      }
      
      # Then do rotation
      rotation_offspring[i,] = rotate_items(rotations[i,])
    }
    else{
      offspring_mutate[i,] = offspring[i,]
      rotation_offspring[i,] = rotations[i,]
    }
  }
  return(list = list(offspring_mutate = offspring_mutate, rotation_offspring=rotation_offspring))
}


update_info_rotate <- function(item_info_OG, rotations) {
  # item_info_OG: original item information (item index, height, width)
  # rotations: a vector of 1 and -1; 1 means no rotation, -1 means swap height and width
  
  # Create a copy of the original item information
  item_info_update <- item_info_OG
  
  # Use logical indexing to update heights and widths where rotation is required
  swap_indices <- rotations == -1
  
  # Swap height and width for items where rotation is -1
  item_info_update[swap_indices, 2:3] <- item_info_OG[swap_indices, c(3, 2)]
  
  return(item_info_update)
}


make_new_pop = function(parent_solution_population, parents_rotations,
                        ranks, constraint_violations, 
                        totalNumBins, crowd_distance, 
                        mutation_prob = 0.5, swap_prob = 0.5){
  
  #Selection - Binary Tournament Selection 
  selection = selection_binary_tourn(parent_solution_population, parents_rotations, ranks, 
                                     constraint_violations, crowd_distance)
  selected_parents = selection$selected_parents
  selected_parents_rotations = selection$selected_rotation
  
  #Crossover
  offspring_together = crossover_uniform(selected_parents, selected_parents_rotations)
  offspring = offspring_together$offspring
  offspring_rotation = offspring_together$offsprings_rotations
  #Mutation
  offspring_mutated = mutation_swap_move(offspring, totalNumBins, offspring_rotation,
                                         mutation_prob, swap_prob)
  
  #Return final offspring pop
  return(offspring_mutated)
}


main_loop = function(Pt, Qt, Pt_rotation, Qt_rotation, m, bin_costs, numCompuslory, item_info,
                     bin_heights, bin_widths, totalNumBins, item_profits, 
                     item_weights, mutation_prob=0.3){
  # Pt and Qt are parent and child solutions with the first column being an index
  
  N = nrow(Pt)
  num_items = ncol(Pt) - 1
  
  Rt = rbind(Pt, Qt)   # Combine the solutions 
  
  #Evaluate the solutions according to the objective functions  
  Rt_eval = matrix(NA, nrow = 2*N, ncol = m + 2)
  Rt_eval[,1] = Rt[,1]    # Index column
  
  Rt_eval[,3] = evaluate_cost(Rt, bin_costs, item_profits, numCompulsory)
  
  all_rotations = rbind(Pt_rotation, Qt_rotation)
  get_violation_and_corners = constraint_violation_calc(solution_pop = Rt, solution_pop_rotations =all_rotations, 
                                                        numCompulsory, item_info,
                                                        bin_heights, bin_widths, totalNumBins)
  Rt_eval[,4] = get_violation_and_corners$violation
  
  Rt_eval[,2] = evaluate_load_imbalance(Rt, all_rotations, item_info,
                                        bin_heights, bin_widths, 
                                        get_violation_and_corners$bottom_corners)
  
  # Perform fast non-dominated sort of the combined population objective 
  # functions Rt to obtain the rank of each solution and therefore 
  # the front it falls into
  fast_non_dom_sort_Rt = fast_non_dom_sort(Rt_eval)
  Fronts = fast_non_dom_sort_Rt$Fronts
  Ranks = fast_non_dom_sort_Rt$Rank
  
  # Initialize next parent pop to be empty with 0 solutions in it
  P_t_plus_1_sols = c()
  P_t_plus_1_rotations = c()
  P_t_plus_1_ranks = c()
  P_t_plus_1_crowd_dist = c()
  P_t_plus_1_constraint_vals = c()
  num_in_P_t_plus_1 = 0
  
  # Initialize counter for fronts, starting at front 1
  i = 1
  
  # Add solutions to next parent pop P_t+1 until next front cannot fit
  while(num_in_P_t_plus_1+length(Fronts[[i]]) <= N){
    
    crowd_dist_F_i = crowding_dist_assign(Rt_eval[Fronts[[i]],-4]) 
    
    solutions_in_front = Rt[Fronts[[i]],]
    
    if(is.null(nrow(solutions_in_front))){
      rep_num = 1
      crowd_dist_of_sol_in_front = crowd_dist_F_i[2]
    }else{
      rep_num = nrow(solutions_in_front)
      crowd_dist_of_sol_in_front = crowd_dist_F_i[,2]
    }
    ranks_of_sol_in_front = rep(i, rep_num)
    
    P_t_plus_1_sols = rbind(P_t_plus_1_sols, solutions_in_front)
    P_t_plus_1_rotations = rbind(P_t_plus_1_rotations, all_rotations[Fronts[[i]],])
    P_t_plus_1_ranks = c(P_t_plus_1_ranks, ranks_of_sol_in_front)
    P_t_plus_1_crowd_dist = c(P_t_plus_1_crowd_dist, crowd_dist_of_sol_in_front)
    P_t_plus_1_constraint_vals = c(P_t_plus_1_constraint_vals, Rt_eval[Fronts[[i]], 4])
    
    num_in_P_t_plus_1 = nrow(P_t_plus_1_sols)
    i = i + 1 #increment i to next front
  }
  # If the next front F_i cannot all fit into P_t_plus_1 we need to 
  # decide which of the solutions in F_i should be added to P_t_plus_1
  # to get to N solutions. The selection is done based on the crowd
  # comparison operator and sort_crowd_comp() is used. 
  
  # To do this the ranks and crowding distances of the solutions in F_i
  # must be calculated so that the correct format can be input into 
  # sort_crowd_comp(). Obviously here the rank will be the same because the 
  # solutions are all in same front. 
  # Create matrix Front_i with first column being an index column, followed by
  # a rank column and then a crowding distance column. 
  
  if(num_in_P_t_plus_1<N){
    Front_i = matrix(NA, nrow = length(Fronts[[i]]), ncol = 3)
    Front_i[,1] = Fronts[[i]]
    Front_i[,2] = Ranks[Fronts[[i]]]
    Front_i[,3] = crowding_dist_assign(Rt_eval[Fronts[[i]],])[,2]
    
    F_i_sort = sort_crowd_comp(Front_i)
    F_i_sort_indices = F_i_sort[,1]
    
    #Only include the solutions needed to get up to N solutions in P_t_plus_1
    solutions_F_i_to_include = F_i_sort_indices[1:(N-num_in_P_t_plus_1)]  
    
    solutions_to_include_from_Rt = Rt[solutions_F_i_to_include,]
    solution_ranks = Front_i[Front_i[,1] %in% solutions_F_i_to_include,2]
    solution_crowd_dist = Front_i[Front_i[,1] %in% solutions_F_i_to_include,3]
    
    P_t_plus_1_sols = rbind(P_t_plus_1_sols, solutions_to_include_from_Rt)
    P_t_plus_1_rotations = rbind(P_t_plus_1_rotations, all_rotations[solutions_F_i_to_include,])
    P_t_plus_1_ranks = c(P_t_plus_1_ranks, solution_ranks)
    P_t_plus_1_crowd_dist = c(P_t_plus_1_crowd_dist, solution_crowd_dist)
    P_t_plus_1_constraint_vals = c(P_t_plus_1_constraint_vals, Rt_eval[solutions_F_i_to_include,4])
  }
  
  row.names(P_t_plus_1_sols) = NULL
  row.names(P_t_plus_1_rotations) = NULL
  row.names(P_t_plus_1_ranks) = NULL
  row.names(P_t_plus_1_crowd_dist) = NULL
  row.names(P_t_plus_1_constraint_vals) = NULL
  
  # Use the parents in P_t_plus_1 to create an offspring population
  Q_t_plus_1 = make_new_pop(P_t_plus_1_sols, P_t_plus_1_rotations,P_t_plus_1_ranks, P_t_plus_1_constraint_vals, 
                            totalNumBins, P_t_plus_1_crowd_dist, mutation_prob, swap_prob)
  
  return(list = list(Parents = P_t_plus_1_sols, 
                     Parents_rotations = P_t_plus_1_rotations,
                     Children= Q_t_plus_1$offspring_mutate,
                     Children_rotations =Q_t_plus_1$rotation_offspring,
                     load_imbalance_mean = mean(Rt_eval[,2]), 
                     cost_mean = mean(Rt_eval[,3])))  
}

run_t_generations = function(t, parents_sol, children_sol,parent_rotations, children_rotations,
                             m, bin_costs, numCompulsory, item_info, bin_heights, bin_widths,
                             totalNumBins, item_profits, item_weights,
                             mutation_prob = 0.3, gen_store = c(100, 200), h = 1){
  
  evals_cost = c()
  evals_load_imbalance = c()
  
  prev_generation_parents = list()
  prev_generation_parents_rotations = list()
  
  pb = txtProgressBar(min = 0, max = t, style = 3) 
  
  for(i in 1:t){
    #print(i)
    next_gen = main_loop(parents_sol, children_sol, parent_rotations, children_rotations,
                         m,  bin_costs, 
                         numCompulsory,item_info, bin_heights, bin_widths, 
                         totalNumBins, item_profits, item_weights)
    
    evals_load_imbalance[i] = next_gen$load_imbalance_mean
    evals_cost[i] = next_gen$cost_mean
    
    parents_sol = next_gen$Parents
    parents_rotations = next_gen$Parents_rotations
    num_parents = nrow(parents_sol)
    parents_sol[,1] = 1:num_parents
    parents_rotations[,1] = 1:num_parents
    
    children_sol = next_gen$Children
    children_rotations = next_gen$Children_rotations
    
    children_sol= cbind(1:num_parents, children_sol)
    children_rotations = cbind(1:num_parents, children_rotations)
    
    if(i %in% gen_store){
      #print("yes")
      gen_filename = paste0("Genstore",h,".txt")
      writeLines(c(paste("gen",i)), con = gen_filename)
      index = which(gen_store==i)
      prev_generation_parents[[index]] = parents_sol
      prev_generation_parents_rotations[[index]] = parents_rotations
    }
    
    setTxtProgressBar(pb, i)
    
  }
  close(pb) 
  
  return(list = list(Parents_t = parents_sol, 
                     Children_t = children_sol, 
                     Parents_t_rotations = parents_rotations,
                     Children_t_rotations = children_rotations,
                     load_imbalance_eval = evals_load_imbalance, 
                     cost_eval = evals_cost,
                     prev_generation_parents = prev_generation_parents,
                     prev_generation_parents_rotations = prev_generation_parents_rotations))
}

get_init_pop_for_GA = function(LP_x, LP_y, total_num_bins, pop_size,
                               num_items, numCompulsory){
  
  initial_pop = matrix(NA, ncol = num_items, nrow = pop_size)
  
  for(s in 1:nrow(initial_pop)){
    if(numCompulsory>0){
      for(i in 1:numCompulsory){
        initial_pop[s,i] = sample(1:total_num_bins, 1, prob = LP_x[i,])
      }
    }
    
    if(numCompulsory!=num_items){
      for(i in (numCompulsory+1):num_items){
        if(sum(LP_x[i,])>0){
          initial_pop[s,i] = sample(1:total_num_bins, 1, prob = LP_x[i,])
        }else{
          initial_pop[s,i] = 0
        }
      }
    }
  }
  
  for(s in 1:nrow(initial_pop)){
    num_items_to_move = sample(1:(num_items/4), 1)
    items_to_move = sample(1:num_items, num_items_to_move, replace = FALSE)
    
    initial_pop[s, items_to_move] = sample(0:total_num_bins, num_items_to_move, 
                                           replace = TRUE)
  }
  
  return(initial_pop)
}