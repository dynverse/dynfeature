#' @rdname calculate_overall_feature_importance
#'
#' @export
calculate_cell_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  if (!is_wrapper_with_waypoints(trajectory)) {
    trajectory <- trajectory %>% dynwrap::add_waypoints()
  }

  waypoints <- trajectory$waypoints

  waypoint_feature_importances <-
    calculate_waypoint_feature_importance(
      trajectory = trajectory,
      expression_source = expression_source,
      waypoints = waypoints,
      fi_method = fi_method,
      verbose = verbose
    )

  geo <- trajectory$waypoints$geodesic_distances

  closest_waypoints <- tibble(
    cell_id = factor(colnames(geo), levels = colnames(geo)),
    waypoint_id = factor(rownames(geo)[apply(geo, 2, which.min)], levels = waypoints$waypoints$waypoint_id %>% sort)
  )

  full_join(
    closest_waypoints,
    waypoint_feature_importances,
    by = "waypoint_id"
  ) %>%
    select(-.data$waypoint_id)
}