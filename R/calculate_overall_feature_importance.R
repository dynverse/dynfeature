#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or \code{caret}.
#' \code{calculate_overall_feature_importance} calculates the importance for the whole trajectory,
#' \code{calculate_milestone_feature_importance} calculates it for individual milestones (e.g. branching points)
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param expression_source The expression data matrix, with features as columns.
#'   * If a matrix is provided, it is used as is.
#'   * If a character is provided, `traj[[expression_source]]` should contain the matrix.
#'   * If a function is provided, that function will be called in order to obtain the expression (useful for lazy loading).
#' @param milestones_oi The milestone(s) for which to calculate feature importance
#' @param waypoints The waypoints, optional
#'
#' @inheritParams calculate_feature_importances
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_overall_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    traj,
    expression_source = "expression",
    fi_method,
    verbose
  ) {
    calculate_milestone_feature_importance(
      traj = traj,
      expression_source = expression_source,
      fi_method = fi_method,
      verbose = verbose
    ) %>%
      group_by(feature_id) %>%
      summarise(importance = mean(importance)) %>%
      arrange(desc(importance))
  }
)