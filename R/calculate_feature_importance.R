#' @rdname calculate_overall_feature_importance
#' @export
calculate_milestone_feature_importance <- function(
  traj,
  expression_source = "expression",
  milestones_oi = NULL,
  method = "ranger",
  method_params = list()
) {
  expression <- get_expression(traj, expression_source)

  # process trajectory
  testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))
  milestone_percentages <- traj$milestone_percentages

  cell_ids <- traj$cell_ids

  testthat::expect_true(all(cell_ids %in% rownames(expression)))

  # process milestones
  if (is.null(milestones_oi)) {
    milestones_oi <- traj$milestone_ids
  }

  milenet_m <- milestone_percentages %>%
    filter(milestone_id %in% milestones_oi) %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>%
    expand_matrix(rownames = cell_ids)

  get_importances(milenet_m, expression, method, method_params) %>% rename(milestone_id = waypoint_id)
}

#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or  \code{caret}. \code{calculate_overall_feature_importance} calculates the importance for the whole trajectory, \code{calculate_milestone_feature_importance} calculates it for individual milestones (eg. branching points)
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param expression_source The expression_source, if not provided will use the expression within the trajectory
#' @param method The method to do regressions, can be `ranger` or any regression model from caret
#' @param method_params Parameters given to the method
#' @param milestones_oi The milestone(s) for which to calculate feature importance
#' @param waypoints The waypoints, optional
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
calculate_overall_feature_importance <- function(
  traj,
  expression_source = "expression",
  method = "ranger",
  method_params = list()
) {
  calculate_milestone_feature_importance(traj, expression_source, method=method, method_params=method_params) %>%
    group_by(feature_id) %>%
    summarise(importance=mean(importance)) %>%
    arrange(desc(importance))
}

#' @rdname calculate_overall_feature_importance
#' @export
calculate_waypoint_feature_importance <- function(
  traj,
  expression_source = "expression",
  waypoints = NULL,
  method = "ranger",
  method_params = list()
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

  get_importances(t(waypoints$geodesic_distances)[rownames(expression),], expression, method, method_params)
}


#' @rdname calculate_overall_feature_importance
#' @export
calculate_cell_feature_importance <- function(
  traj,
  expression_source = "expression",
  method = "ranger",
  method_params = list()
) {
  if(!is_wrapper_with_waypoints(traj)) {
    traj <- traj %>% dynwrap::add_waypoints()
  }

  waypoint_feature_importances <- calculate_waypoint_feature_importance(traj, expression_source, waypoints=NULL, method, method_params)

  closest_waypoints <- traj$waypoints$geodesic_distances %>% {
    tibble(
      cell_id = colnames(.),
      waypoint_id = rownames(.)[apply(., 2, which.min)]
    )
  }

  cell_feature_importances <- full_join(
    closest_waypoints,
    waypoint_feature_importances,
    "waypoint_id"
  ) %>% select(-waypoint_id)

  cell_feature_importances
}

#' @rdname calculate_overall_feature_importance
#' @export
calculate_branch_feature_importance <- function(
  traj,
  expression_source = "expression",
  method = "ranger",
  method_params = list()
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

  get_importances(edge_membership, expression_source, method, method_params) %>%
    left_join(milestone_network, c("waypoint_id"="edge_id")) %>%
    select(-waypoint_id)
}



#' @rdname calculate_overall_feature_importance
#' @export
calculate_branching_point_feature_importance <- function(
  traj,
  expression_source = "expression",
  milestones_oi = traj$milestone_ids,
  method = "ranger",
  method_params = list()
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

  map_df(milestones_oi, function(milestone_oi) {
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
      get_importances(outcome, expression_oi, method, method_params) %>%
        select(feature_id, importance) %>%
        mutate(milestone_id = milestone_oi)
    }
  })
}






get_importance <- function(data, expression, method, method_params) {
  if (method == "ranger") {
    requireNamespace("ranger")
    # process ranger params

    default_params <- list(
      dependent.variable.name="PREDICT",
      data = data,
      importance="impurity",
      mtry = function(x) ncol(x) * .01
    )
    method_params <- list_modify(default_params, !!!method_params)
    if (is.function(method_params$mtry)) {method_params$mtry <- method_params$mtry(expression)}

    # call ranger and get variable importance
    do.call(ranger::ranger, method_params)$variable.importance
  } else {
    requireNamespace("caret")

    default_params <- list(form=PREDICT~., data=data, method=method, trControl=trainControl(method="none"))
    method_params <- list_modify(default_params, !!!method_params)

    if(!method %in% caret::modelLookup()$model) {stop("Invalid method")}

    model <- do.call(caret::train, method_params)
    caret::varImp(model)[[1]] %>% {set_names(.[, 1], rownames(.))}
#
#     microbenchmark::microbenchmark(lm(PREDICT~., data))
#     microbenchmark::microbenchmark(caret::train(PREDICT~., data, "lm"))
#
#     caret::train(data[, -1], data[, 1], "lm", trControl=trainControl(method="none"), tuneGrid=NULL)
  }

  # regr_task <- makeRegrTask("hi", data, "PREDICT")
  # learner <- makeLearner("regr.randomForest")
  #
  # generateFeatureImportanceData(regr_task, learner=learner)
}


get_importances <- function(outcome, expression, method, method_params) {
  if(!is.matrix(outcome)) {
    outcome <- matrix(outcome, ncol=1)
    colnames(outcome) <- "1"
  }

  outcome <- outcome[,apply(outcome, 2, function(x) length(unique(x)) > 1), drop=F]
  importances <- map_df(seq_len(ncol(outcome)), function(i) {
    data <-
      outcome[,i, drop=F] %>%
      magrittr::set_colnames("PREDICT") %>%
      cbind(expression) %>%
      as.data.frame()

    importance <- get_importance(data, expression, method, method_params)

    data_frame(waypoint_id = colnames(outcome)[[i]], feature_id = names(importance), importance)
  })

  importances %>%
    arrange(desc(importance))
}
