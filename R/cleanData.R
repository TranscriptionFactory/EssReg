
cleanData <- function(xdata, ydata) {

  # make sure in same order
  y_correct_order <- match(rownames(xdata), rownames(ydata))
  ydata <- ydata[y_correct_order,]
  
  # get median cols
  empty_cols <- which(apply(xdata, 2, median) == 0)
  empty_rows <- which(apply(xdata, 1, median) == 0)
  
  # remove zero SD too
  zero_sd <- which(apply(xdata, 2, sd) == 0)
  
  bad_cols <- base::union(empty_cols, zero_sd)
  
  # check if we need to remove any rows
  if (length(empty_rows) > 0) {
    # remove rows
    xdata <- xdata[-empty_rows,]
    ydata <- ydata[-empty_rows]
  }
  
   # check if we need to remove any columns
  if (length(bad_cols) > 0) {
    xdata <- xdata[, -bad_cols]
  }
  
  return(list("x" = xdata, "y" = ydata))
}
