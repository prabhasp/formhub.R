outliers <- function(col, log=FALSE, multiplier=3, which="both") {
  ccol <- if(log) { log(abs(col+1))} else { col}
  logstring <- if(log) {"log() "} else { "" }
  mean <- mean(ccol, na.rm=TRUE)
  stdev <- sqrt(var(ccol, na.rm=TRUE))
  print(paste(logstring, "mean: ", mean, " stdev: ", stdev))
  dir_fn <- function(x) { if(which=="right") {x} else if (which=="left") {-x}
                          else (abs(x)) }
  print(paste("num outliers:", length(which(dir_fn(ccol-mean) > multiplier*stdev))))
  col[which(dir_fn(ccol-mean) > multiplier*stdev)]
}
qplot(log(merged_education2$num_benches))
outliers(merged_education2$num_benches, log=TRUE, which="right")
rightlogoutliers <- function(c) { length(outliers(c, log=TRUE, which="right", multiplier=4))}
numcolwise(rightlogoutliers)(merged_education2)