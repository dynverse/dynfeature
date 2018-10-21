#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_waypoint_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    traj,
    expression_source = "expression",
    waypoints = NULL,
    fi_method,
    verbose
  ) {
    if (is.null(waypoints)) {
      if (!is_wrapper_with_waypoints(traj)) {
        message("Adding waypoints to prediction")
        traj <- traj %>% dynwrap::add_waypoints()
      }

      waypoints <- traj$waypoints
    }

    expression <- get_expression(traj, expression_source)

    calculate_feature_importances(
      X = expression,
      Y = t(waypoints$geodesic_distances)[rownames(expression),],
      fi_method = fi_method,
      verbose = verbose
    ) %>%
      rename(waypoint_id = predictor_id)
  }
)