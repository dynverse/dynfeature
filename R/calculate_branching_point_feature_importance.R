#' @rdname calculate_overall_feature_importance
#' @export
calculate_branching_point_feature_importance <- function(
  traj,
  expression_source = "expression",
  milestones_oi = traj$milestone_ids,
  method = "ranger",
  method_params = list(),
  verbose = FALSE
) {

  milestone_network <- traj$milestone_network %>%
    mutate(edge_id = as.character(row_number())) %>%
    select(from, to, edge_id)

  edge_membership <- traj$progressions %>%
    group_by(cell_id) %>%
    top_n(1, percentage) %>%
    ungroup() %>%
    left_join(milestone_network, c("from", "to")) %>%
    reshape2::acast(cell_id~edge_id, value.var="percentage") %>%
    is.na %>%
    !.

  expression <- get_expression(traj, expression_source)

  map_df(
    seq_along(milestones_oi), function(i) {
      if (verbose) cat("Processing milestone ", i , "/", length(milestones_oi), "\n", sep = "")
      milestone_oi <- milestones_oi[[i]]
      # select the cells which are close to the milestone
      prog <- traj$progressions %>%
        filter(from == milestone_oi | to == milestone_oi) %>%
        mutate(milestone_other = ifelse(from == milestone_oi, to, from)) %>%
        group_by(cell_id) %>%
        top_n(1, percentage) %>%
        ungroup()

      expression_oi <- expression[prog$cell_id, ]
      outcome <- as.character(prog$milestone_other)

      if(length(unique(prog$milestone_other)) < 2) {
        warning("Could not find features specific for milestone ", milestone_oi)
        tibble()
      } else {
        get_importances(outcome, expression_oi, method, method_params, verbose) %>%
          select(feature_id, importance) %>%
          mutate(milestone_id = milestone_oi)
      }
    })
}