#' @rdname calculate_overall_feature_importance
#'
#' @export
calculate_branching_point_feature_importance <- function(
  trajectory,
  expression_source = "expression",
  milestones_oi = trajectory$milestone_ids,
  fi_method = fi_ranger_rf_lite(),
  verbose = FALSE
) {
  # assign name to each edge
  milestone_network <-
    trajectory$milestone_network %>%
    mutate(edge_id = as.character(row_number())) %>%
    select(.data$from, .data$to, .data$edge_id)

  # determine which cell is part of which edge
  edge_membership <- trajectory$progressions %>%
    group_by(.data$cell_id) %>%
    top_n(1, .data$percentage) %>%
    ungroup() %>%
    left_join(milestone_network, c("from", "to")) %>%
    mutate(contains = TRUE) %>%
    reshape2::acast(cell_id~edge_id, value.var = "contains", fill = FALSE)

  expression <- get_expression(trajectory, expression_source)

  map_df(
    seq_along(milestones_oi),
    function(i) {
      if (verbose) cat("Processing milestone ", i , "/", length(milestones_oi), "\n", sep = "")

      milestone_oi <- milestones_oi[[i]]

      # select the cells which are close to the milestone
      prog <-
        trajectory$progressions %>%
        filter(.data$from == milestone_oi | .data$to == milestone_oi) %>%
        mutate(milestone_other = ifelse(.data$from == milestone_oi, .data$to, .data$from)) %>%
        group_by(.data$cell_id) %>%
        top_n(1, .data$percentage) %>%
        ungroup()

      expression_oi <- expression[prog$cell_id, ]
      outcome <- as.character(prog$milestone_other)

      if(length(unique(prog$milestone_other)) < 2) {
        warning("Could not find features specific for milestone ", milestone_oi)
        tibble()
      } else {
        calculate_feature_importances(
          X = expression_oi,
          Y = outcome,
          fi_method = fi_method,
          verbose = verbose
        ) %>%
          transmute(
            milestone_id = factor(milestone_oi, levels = trajectory$milestone_ids),
            .data$feature_id,
            .data$importance
          )
      }
    }
  )
}