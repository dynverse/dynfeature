#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_cell_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    traj,
    expression_source = "expression",
    fi_method,
    verbose
  ) {
    if (!is_wrapper_with_waypoints(traj)) {
      traj <- traj %>% dynwrap::add_waypoints()
    }

    waypoints <- traj$waypoints

    waypoint_feature_importances <-
      calculate_waypoint_feature_importance(
        traj = traj,
        expression_source = expression_source,
        waypoints = waypoints,
        fi_method = fi_method,
        verbose = verbose
      )

    closest_waypoints <-
      traj$waypoints$geodesic_distances %>% {
        tibble(
          cell_id = factor(colnames(.), levels = colnames(.)),
          waypoint_id = factor(rownames(.)[apply(., 2, which.min)], levels = waypoints$waypoints$waypoint_id %>% sort)
        )
      }

    full_join(
      closest_waypoints,
      waypoint_feature_importances,
      by = "waypoint_id"
    ) %>%
      select(-waypoint_id)
  }
)