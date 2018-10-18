get_importances <- function(outcome, expression, method, method_params, verbose = FALSE) {
  if(!is.matrix(outcome)) {
    outcome <- matrix(outcome, ncol = 1)
    colnames(outcome) <- "1"
  }

  outcome <- outcome[,apply(outcome, 2, function(x) length(unique(x)) > 1), drop=F]
  importances <- map_df(seq_len(ncol(outcome)), function(i) {
    if (verbose) cat("Generating forest ", i , "/", ncol(outcome), "\n", sep = "")
    data <-
      outcome[,i, drop=F] %>%
      magrittr::set_colnames("PREDICT") %>%
      cbind(expression) %>%
      as.data.frame()

    importance <- get_importance(
      data = data,
      expression = expression,
      method = method,
      method_params = method_params,
      verbose = verbose
    )

    data_frame(waypoint_id = colnames(outcome)[[i]], feature_id = names(importance), importance)
  })

  importances %>%
    arrange(desc(importance))
}
