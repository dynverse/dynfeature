#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_branch_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    trajectory,
    expression_source = "expression",
    fi_method,
    verbose
  ) {
    # assign name to each edge
    milestone_network <-
      trajectory$milestone_network %>%
      mutate(edge_id = as.character(row_number())) %>%
      select(from, to, edge_id)

    # determine which cell is part of which edge
    edge_membership <-
      trajectory$progressions %>%
      group_by(cell_id) %>%
      top_n(1, percentage) %>%
      ungroup() %>%
      left_join(milestone_network, c("from", "to")) %>%
      reshape2::acast(cell_id ~ edge_id, value.var = "percentage") %>%
      {!is.na(.)}

    expression <- get_expression(trajectory, expression_source)


    out <- calculate_feature_importances(
      X = expression,
      Y = edge_membership,
      fi_method = fi_method,
      verbose = verbose
    )
    suppressWarnings({
      out <- out %>%
        left_join(milestone_network, c("predictor_id" = "edge_id"))
    })

    out %>%
      transmute(
        feature_id,
        from = factor(from, trajectory$milestone_ids),
        to = factor(to, trajectory$milestone_ids),
        importance
      )
  }
)