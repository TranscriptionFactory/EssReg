#' @export
cleanData <- function(xdata, ydata) {

  # make sure in same order
  y_correct_order <- match(rownames(xdata), rownames(ydata))
  ydata <- ydata[y_correct_order]

  # fix ydata rownames
  ydata <- as.matrix(ydata)
  rownames(ydata) <- rownames(xdata)

  # remove zero SD too
  zero_sd <- which(apply(xdata, 2, sd) == 0)

   # check if we need to remove any columns
  if (length(zero_sd) > 0) {
    xdata <- xdata[, -zero_sd]
  }

  return(list("x" = xdata, "y" = ydata))
}
