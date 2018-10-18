#' @rdname calculate_overall_feature_importance
#' @export
calculate_branch_feature_importance <- function(
  traj,
  expression_source = "expression",
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

  get_importances(edge_membership, expression_source, method, method_params, verbose) %>%
    left_join(milestone_network, c("waypoint_id"="edge_id")) %>%
    select(-waypoint_id)
}