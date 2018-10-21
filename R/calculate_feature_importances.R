#' Calculate feature importance scores
#'
#' @param X A data frame containing the features as columns.
#' @param Y A data frame of predictor variables, with `nrow(Y) == nrow(X)`.
#' @param fi_method A feature importance method. Default: `fi_ranger_rf_lite()`. Check `?fi_methods` for a full list of available feature importance methods.
#' @param verbose Whether to print out extra information.
calculate_feature_importances <- function(X, Y, fi_method = fi_ranger_rf_lite(), verbose = FALSE) {
  # if Y is a vector or a matrix, turn it into a data frame
  if (!is.data.frame(Y)) {
    Y <- as_data_frame(Y)
  }

  # calculate importance score for each predictor separately
  importances <- map_df(seq_len(ncol(Y)), function(i) {
    if (verbose) cat("Generating forest ", i , "/", ncol(Y), "\n", sep = "")

    y <- Y %>% pull(!!i)

    # return 0 if y are all the same values
    if (length(unique(y)) == 1) {
      importance <- set_names(rep(0, ncol(X)), colnames(X))
    } else {
      importance <- fi_method$fun(X, y, verbose = verbose)
    }

    data_frame(
      predictor_id = factor(colnames(Y)[[i]], levels = colnames(Y)),
      feature_id = factor(names(importance), levels = names(importance)),
      importance
    )
  })

  # return importances ordered by value
  importances %>%
    arrange(desc(importance))
}