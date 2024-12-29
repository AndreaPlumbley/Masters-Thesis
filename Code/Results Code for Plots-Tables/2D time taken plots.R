overall_time = list()
times = c()
count = 1
for(i in c(41,42)){
  for(j in 6:10){
    file_name = paste0("Results_Combination", i,".",j, ".txt")
    file_content = readLines(file_name)
    info_time = tail(file_content, n =1)
    number <- as.numeric(gsub(".*: ([0-9.]+) sec.*", "\\1", info_time))
    times[count] = number
    count = count +1
  }
}


overall_time[[4]] = times/60


boxplot(overall_time,
        names = c("20", "50", "100", "200"),  # Labels for each plot
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),  # Colors for each boxplot
        las = 1,
        xlab ="Number of items",
        ylab = "Time (minutes)",
        cex.lab = 1.5)



