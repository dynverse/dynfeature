#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_milestone_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    traj,
    expression_source = "expression",
    milestones_oi = NULL,
    fi_method,
    verbose
  ) {
    # get expression from trajectory source or expression source
    expression <- get_expression(traj, expression_source)

    # check trajectory and extract some variables
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))
    milestone_percentages <- traj$milestone_percentages
    cell_ids <- traj$cell_ids

    testthat::expect_true(all(cell_ids %in% rownames(expression)))
    testthat::expect_true(length(cell_ids) >= 3, info = "Need 3 or more cells in a trajectory to determine important features")

    # process milestones
    if (is.null(milestones_oi)) {
      milestones_oi <- traj$milestone_ids
    }

    # construct milestone percentages matrix:
    # * only retain relevant milestones
    # * transform from long format to matrix
    # * if cells were filtered out, add them back to the matrix
    milenet_m <-
      milestone_percentages %>%
      filter(milestone_id %in% milestones_oi) %>%
      reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>%
      expand_matrix(rownames = cell_ids)

    # calculate feature importance scores
    calculate_feature_importances(
      X = expression,
      Y = milenet_m,
      fi_method = fi_method,
      verbose = verbose
    ) %>%
      rename(milestone_id = predictor_id)
  }
)