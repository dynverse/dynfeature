#' @rdname calculate_overall_feature_importance
#' @export
calculate_milestone_feature_importance <- function(
  traj,
  expression_source = "expression",
  milestones_oi = NULL,
  method = "ranger",
  method_params = list(),
  verbose = FALSE
) {
  expression <- get_expression(traj, expression_source)

  # process trajectory
  testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))
  milestone_percentages <- traj$milestone_percentages

  cell_ids <- traj$cell_ids

  testthat::expect_true(all(cell_ids %in% rownames(expression)))

  # process milestones
  if (is.null(milestones_oi)) {
    milestones_oi <- traj$milestone_ids
  }

  if (nrow(milestone_percentages) < 2) {
    stop("Need 3 or more cells in a trajectory to determine important features")
  }

  milenet_m <- milestone_percentages %>%
    filter(milestone_id %in% milestones_oi) %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>%
    expand_matrix(rownames = cell_ids)

  get_importances(milenet_m, expression, method, method_params, verbose) %>% rename(milestone_id = waypoint_id)
}