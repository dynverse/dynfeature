#' @rdname calculate_overall_feature_importance
#'
#' @export
calculate_branch_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  # assign name to each edge
  milestone_network <-
    trajectory$milestone_network %>%
    mutate(edge_id = as.character(row_number())) %>%
    select(.data$from, .data$to, .data$edge_id)

  # determine which cell is part of which edge
  edge_membership <-
    trajectory$progressions %>%
    group_by(.data$cell_id) %>%
    top_n(1, .data$percentage) %>%
    ungroup() %>%
    left_join(milestone_network, c("from", "to")) %>%
    mutate(contains = TRUE) %>%
    reshape2::acast(cell_id ~ edge_id, value.var = "contains", fill = FALSE)

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
      .data$feature_id,
      from = factor(.data$from, trajectory$milestone_ids),
      to = factor(.data$to, trajectory$milestone_ids),
      .data$importance
    )
}