#' @rdname calculate_overall_feature_importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_branching_point_feature_importance <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    trajectory,
    expression_source = "expression",
    milestones_oi = trajectory$milestone_ids,
    fi_method,
    verbose
  ) {
    # assign name to each edge
    milestone_network <-
      trajectory$milestone_network %>%
      mutate(edge_id = as.character(row_number())) %>%
      select(from, to, edge_id)

    # determine which cell is part of which edge
    edge_membership <- trajectory$progressions %>%
      group_by(cell_id) %>%
      top_n(1, percentage) %>%
      ungroup() %>%
      left_join(milestone_network, c("from", "to")) %>%
      reshape2::acast(cell_id~edge_id, value.var="percentage") %>%
      {!is.na(.)}

    expression <- get_expression(trajectory, expression_source)

    map_df(
      seq_along(milestones_oi),
      function(i) {
        if (verbose) cat("Processing milestone ", i , "/", length(milestones_oi), "\n", sep = "")

        milestone_oi <- milestones_oi[[i]]

        # select the cells which are close to the milestone
        prog <-
          trajectory$progressions %>%
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
          calculate_feature_importances(
            X = expression_oi,
            Y = outcome,
            fi_method = fi_method,
            verbose = verbose
          ) %>%
            transmute(
              milestone_id = factor(milestone_oi, levels = trajectory$milestone_ids),
              feature_id,
              importance
            )
        }
      }
    )
  }
)