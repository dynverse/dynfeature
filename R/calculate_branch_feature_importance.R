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


#' @rdname calculate_overall_feature_importance
#'
#' @param pct_around Percentage of upstream and downstream cells w.r.t. current branch length that will also be included in branch feature importance
#'
#' @export
#'
#' @include calculate_feature_importances.R
calculate_branch_feature_importance2 <- inherit_default_params(
  super_functions = list(calculate_feature_importances),
  fun = function(
    trajectory,
    expression_source = "expression",
    fi_method,
    pct_around = .5,
    verbose
  ) {
    if (pct_around > 0) {
      # determine which cell is part of which edge
      geod_after <- dynwrap::calculate_geodesic_distances(
        trajectory,
        waypoint_cells = character(0),
        waypoint_milestone_percentages = tibble(
          waypoint_id = paste0("FIMPWP_", trajectory$milestone_ids),
          milestone_id = trajectory$milestone_ids,
          percentage = 1
        ),
        directed = "forward"
      ) %>%
        reshape2::melt(varnames = c("milestone_id", "cell_id"), value.name = "distance") %>%
        mutate(milestone_id = gsub("^FIMPWP_", "", milestone_id), cell_id = as.character(cell_id))
      geod_before <- dynwrap::calculate_geodesic_distances(
        trajectory,
        waypoint_cells = character(0),
        waypoint_milestone_percentages = tibble(
          waypoint_id = paste0("FIMPWP_", trajectory$milestone_ids),
          milestone_id = trajectory$milestone_ids,
          percentage = 1
        ),
        directed = "reverse"
      ) %>%
        reshape2::melt(varnames = c("milestone_id", "cell_id"), value.name = "distance") %>%
        mutate(milestone_id = gsub("^FIMPWP_", "", milestone_id), cell_id = as.character(cell_id))
    }

    expression <- get_expression(trajectory, expression_source)

    out <- pmap_df(
      trajectory$milestone_network,
      function(from, to, length, ...) {

        on_branch <-
          trajectory$progressions %>%
          filter(from == !!from, to == !!to) %>%
          select(cell_id, percentage)

        if (pct_around > 0) {
          on_branch <-
            bind_rows(
              on_branch,
              geod_before %>%
                filter(milestone_id == from, distance > 0, distance / length <= pct_around) %>%
                transmute(cell_id, percentage = -distance / length),
              geod_after %>%
                filter(milestone_id == to, distance > 0, distance / length <= pct_around) %>%
                transmute(cell_id, percentage = distance / length)
            )
        }

        calculate_feature_importances(
          X = expression[on_branch$cell_id,],
          Y = on_branch$percentage,
          fi_method = fi_method,
          verbose = verbose
        ) %>%
          transmute(feature_id, from, to, importance)
      }
    )

  }
)