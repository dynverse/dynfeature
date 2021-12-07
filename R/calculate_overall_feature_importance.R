#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or \code{caret}.
#' \code{calculate_overall_feature_importance} calculates the importance for the whole trajectory,
#' \code{calculate_milestone_feature_importance} calculates it for individual milestones (e.g. branching points)
#'
#' @param trajectory A trajectory object containing expression values and a trajectory.
#' @param expression_source The expression data matrix, with features as columns.
#'   * If a matrix is provided, it is used as is.
#'   * If a character is provided, `trajectory[[expression_source]]` should contain the matrix.
#'   * If a function is provided, that function will be called in order to obtain the expression (useful for lazy loading).
#' @param milestones_oi The milestone(s) for which to calculate feature importance
#' @param waypoints The waypoints, optional
#' @param fi_method A feature importance method. Default: `fi_ranger_rf_lite()`. Check `?fi_methods` for a full list of available feature importance methods.
#' @param verbose Whether to print out extra information.
#'
#' @returns A data frame with two or more columns, `feature_id`, and `importance`. `feature_id` is a column in the trajectory expression matrix. Additional columns may be available depending on the function called.
#'
#' @importFrom ranger ranger
#'
#' @export
#'
#' @examples
#' library(dynwrap)
#' data(example_trajectory)
#'
#' calculate_overall_feature_importance(example_trajectory)
calculate_overall_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  calculate_milestone_feature_importance(
    trajectory = trajectory,
    expression_source = expression_source,
    fi_method = fi_method,
    verbose = verbose
  ) %>%
    group_by(.data$feature_id) %>%
    summarise(importance = mean(.data$importance)) %>%
    arrange(desc(.data$importance))
}