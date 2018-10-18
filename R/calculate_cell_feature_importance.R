#' @rdname calculate_overall_feature_importance
#' @export
calculate_cell_feature_importance <- function(
  traj,
  expression_source = "expression",
  method = "ranger",
  method_params = list(),
  verbose = FALSE
) {
  if(!is_wrapper_with_waypoints(traj)) {
    traj <- traj %>% dynwrap::add_waypoints()
  }

  waypoint_feature_importances <- calculate_waypoint_feature_importance(
    traj,
    expression_source,
    waypoints = NULL,
    method,
    method_params,
    verbose
  )

  closest_waypoints <- traj$waypoints$geodesic_distances %>% {
    tibble(
      cell_id = colnames(.),
      waypoint_id = rownames(.)[apply(., 2, which.min)]
    )
  }

  cell_feature_importances <- full_join(
    closest_waypoints,
    waypoint_feature_importances,
    "waypoint_id"
  ) %>% select(-waypoint_id)

  cell_feature_importances
}