#' @rdname calculate_overall_feature_importance
#'
#' @export
calculate_milestone_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  milestones_oi = NULL,
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  # get expression from trajectory source or expression source
  expression <- dynwrap::get_expression(trajectory, expression_source)

  # check trajectory and extract some variables
  testthat::expect_true(dynwrap::is_wrapper_with_trajectory(trajectory))
  milestone_percentages <- trajectory$milestone_percentages
  cell_ids <- trajectory$cell_ids

  testthat::expect_true(all(cell_ids %in% rownames(expression)))
  testthat::expect_true(length(cell_ids) >= 3, info = "Need 3 or more cells in a trajectory to determine important features")

  # process milestones
  if (is.null(milestones_oi)) {
    milestones_oi <- trajectory$milestone_ids
  }

  # construct milestone percentages matrix:
  # * only retain relevant milestones
  # * transform from long format to matrix
  # * if cells were filtered out, add them back to the matrix
  milenet_m <-
    milestone_percentages %>%
    filter(.data$milestone_id %in% milestones_oi) %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>%
    dynutils::expand_matrix(rownames = cell_ids)

  # calculate feature importance scores
  calculate_feature_importances(
    X = expression,
    Y = milenet_m,
    fi_method = fi_method,
    verbose = verbose
  ) %>%
    rename(milestone_id = .data$predictor_id)
}