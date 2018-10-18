#' @rdname calculate_overall_feature_importance
#' @export
calculate_waypoint_feature_importance <- function(
  traj,
  expression_source = "expression",
  waypoints = NULL,
  method = "ranger",
  method_params = list(),
  verbose = FALSE
) {
  if(is.null(waypoints)) {
    if(is_wrapper_with_waypoints(traj)) {
      waypoints <- traj$waypoints
    } else {
      message("Adding waypoints to prediction")

      traj <- traj %>% dynwrap::add_waypoints()
      waypoints <- traj$waypoints
    }
  }

  expression <- get_expression(traj, expression_source)

  get_importances(t(waypoints$geodesic_distances)[rownames(expression),], expression, method, method_params, verbose)
}