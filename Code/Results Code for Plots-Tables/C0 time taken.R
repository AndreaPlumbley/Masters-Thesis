
times = list()
for (j in 0:3){
  time = c()
  for (i in 0:9){
    file_name = paste0("R_C0_prob_1_A", j, "_0_", i, ".txt")
    file_content = readLines(file_name)
    info_time = tail(file_content, n =1)
    number <- as.numeric(gsub(".*: ([0-9.]+) sec.*", "\\1", info_time))
    time[i+1] = number
  }
  times[[j+1]] = time
}

boxplot(times, 
        main = "Multiple Boxplots",  # Title for the whole plot
        names = c("Plot 1", "Plot 2", "Plot 3", "Plot 4"),  # Labels for each plot
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),  # Colors for each boxplot
        las = 2)
