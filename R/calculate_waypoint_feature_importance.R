#' @rdname calculate_overall_feature_importance
#'
#' @export
calculate_waypoint_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  waypoints = NULL,
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  if (is.null(waypoints)) {
    if (!is_wrapper_with_waypoints(trajectory)) {
      message("Adding waypoints to prediction")
      trajectory <- trajectory %>% dynwrap::add_waypoints()
    }

    waypoints <- trajectory$waypoints
  }

  expression <- get_expression(trajectory, expression_source)

  calculate_feature_importances(
    X = expression,
    Y = t(waypoints$geodesic_distances)[rownames(expression),unique(rownames(waypoints$geodesic_distances))],
    fi_method = fi_method,
    verbose = verbose
  ) %>%
    rename(waypoint_id = .data$predictor_id)
}