#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or \code{caret}.
#' \code{calculate_overall_feature_importance} calculates the importance for the whole trajectory,
#'  \code{calculate_milestone_feature_importance} calculates it for individual milestones (eg. branching points)
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param expression_source The expression_source, if not provided will use the expression within the trajectory
#' @param method The method to do regressions, can be `ranger` or any regression model from caret
#' @param method_params Parameters given to the method
#' @param milestones_oi The milestone(s) for which to calculate feature importance
#' @param waypoints The waypoints, optional
#' @param verbose Whether or not to print helpful messages.
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
calculate_overall_feature_importance <- function(
  traj,
  expression_source = "expression",
  method = "ranger",
  method_params = list(),
  verbose = FALSE
) {
  calculate_milestone_feature_importance(
    traj = traj,
    expression_source = expression_source,
    method = method,
    method_params = method_params,
    verbose = verbose
  ) %>%
    group_by(feature_id) %>%
    summarise(importance=mean(importance)) %>%
    arrange(desc(importance))
}