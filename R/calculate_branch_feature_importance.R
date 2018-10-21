#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_branch_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    traj,
    expression_source = "expression",
    fi_method,
    verbose
  ) {
    # assign name to each edge
    milestone_network <-
      traj$milestone_network %>%
      mutate(edge_id = as.character(row_number())) %>%
      select(from, to, edge_id)

    # determine which cell is part of which edge
    edge_membership <-
      traj$progressions %>%
      group_by(cell_id) %>%
      top_n(1, percentage) %>%
      ungroup() %>%
      left_join(milestone_network, c("from", "to")) %>%
      reshape2::acast(cell_id ~ edge_id, value.var = "percentage") %>%
      {!is.na(.)}

    expression <- get_expression(traj, expression_source)

    calculate_feature_importances(
      X = expression_source,
      Y = edge_membership,
      fi_method = fi_method,
      verbose = verbose
    ) %>%
      left_join(milestone_network, c("feature_id" = "edge_id")) %>%
      select(-feature_id)
  }
)