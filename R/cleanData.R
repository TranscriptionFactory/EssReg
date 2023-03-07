#' @export
cleanData <- function(xdata, ydata, mode = 0,
                      quantile_filter = 0.0, remove_zero_median_cols = F,
                      remove_zero_median_rows = F,
                      er_input = NULL) {

  if (!is.null(er_input)) {
    mode = er_input$mode
    quantile_filter = ifelse(is.null(er_input$quantile_filter), 0.0, er_input$quantile_filter)
    remove_zero_median_cols = ifelse(is.null(er_input$remove_zero_median_cols), F, er_input$remove_zero_median_cols)
    remove_zero_median_rows = ifelse(is.null(er_input$remove_zero_median_rows), F, er_input$remove_zero_median_rows)
  }

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

  if (mode > 0 & quantile_filter >= 0) {
    # filter quantiles and remove empty rows

    empty_cols <- which(apply(xdata, 2, median) == 0)

    colvars <- apply(xdata, 2, var)

    low_var_cols <- which(colvars < quantile(colvars, quantile_filter))

    if (remove_zero_median_cols) {
      remove_cols <- base::union(empty_cols, low_var_cols)
    } else {
      remove_cols <- low_var_cols
    }

    if (length(remove_cols > 0)) {
      xdata <- xdata[, -remove_cols]
    }

    if (remove_zero_median_rows) {
      # remove empty rows last
      empty_rows <- which(apply(xdata, 1, median) == 0)

      # check if we need to remove any rows
      if (length(empty_rows) > 0) {
        # remove rows
        xdata <- xdata[-empty_rows,]
        ydata <- ydata[-empty_rows]
      }
    }
  }

  return(list("x" = xdata, "y" = ydata))
}
