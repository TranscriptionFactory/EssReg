#' @export
checkInputData <- function(er_input) {

  ## Data Housekeeping #########################################################
  raw_y <- as.matrix(read.csv(er_input$y_path, row.names = 1))
  raw_x <- as.matrix(read.csv(er_input$x_path, row.names = 1))  # input raw x is now raw_x

  ## standardization but only x
  x <- scale(raw_x, T, T) # scaled x is now x

  if (er_input$std_y) {
    y <- scale(y, T, T)
  }


  opt_lambda <- er_input$lambda ## no longer CV lambda
  ## ER Housekeeping ###########################################################
  n <- nrow(x);  p <- ncol(x) #### feature matrix dimensions
  se_est <- apply(x, 2, stats::sd) #### get sd of columns for feature matrix

  sigma <- cor(x)


  #### scale delta by this value to satisfy some requirements so that the
  #### statistical guarantees in the paper hold
  delta_scaled <- er_input$delta * sqrt(log(max(p, n)) / n)

  msg = " "
  prms = list()

  res_sigma = tryCatch({

    msg = "error during threshSigma"

    prms = list("er_input$thresh_fdr" = er_input$thresh_fdr,
                "x" = x,
                "sigma" = sigma)
    #### threshold sigma to control for FDR
    if (!is.null(er_input$thresh_fdr)) {
      control_fdr <- threshSigma(x = x,
                                 sigma = sigma,
                                 thresh = er_input$thresh_fdr)
      sigma <- control_fdr$thresh_sigma
      kept_entries <- control_fdr$kept_entries
    } else {
      kept_entries <- matrix(1, nrow = nrow(sigma), ncol = ncol(sigma))
    }
  }, warning = function(w) {

    cat("warning during ")
    cat(msg)
    cat("\n")
    return(prms)
  }, error = function(e) {

    cat("error during ")
    cat(msg)
    cat("\n")
    return(prms)
  }, finally = {

    cat("treshSigma finished\n")
  })

  res_delta = tryCatch({

    msg = "error during cvDelta"

    prms = list("delta_scaled" = delta_scaled,
                "rep_cv" = rep_cv,
                "raw_x" = raw_x,
                "kept_entries" = kept_entries)

    ## Delta Cross-Validation ####################################################
    #### if delta has more than 1 element, then do rep_CV # of replicates
    #### of CV_Delta and select median of replicates
    #### use the unstandardized version of x (if available) to avoid signal leakage in CV
    {
    #   foreach::foreach(i = 1:rep_cv, .combine = c) %dopar% {
    #     cvDelta(raw_x = raw_x,
    #             fdr_entries = kept_entries,
    #             deltas_scaled = delta_scaled)
    #   } -> cv_delta_reps
    #   opt_delta <- stats::median(cv_delta_reps)
    # } else {
    #   opt_delta <- delta_scaled
    # }
    cv_delta_reps = c()
    for (i in 1:er_input$rep_cv){
        cvDelta(raw_x = raw_x,
                fdr_entries = kept_entries,
                deltas_scaled = delta_scaled)
    }
      opt_delta <- stats::median(cv_delta_reps)
    }

  }, warning = function(w) {

    cat("warning during ")
    cat(msg)
    cat("\n")
    return(prms)

  }, error = function(e) {

    cat("warning during ")
    cat(msg)
    cat("\n")
    return(prms)

  }, finally = {

    cat("cvDelta finished \n")

  })

  return(list("threshSigma" = res_sigma, "cvDelta" = res_delta))
}
