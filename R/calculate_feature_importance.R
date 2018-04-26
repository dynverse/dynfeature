#' @rdname calculate_overall_feature_importance
#' @export
calculate_milestone_feature_importance <- function(
  traj,
  expression = NULL,
  milestones_oi = NULL,
  method = "ranger",
  method_params = list()
) {
  # process expression
  if (is.null(expression)) {
    testthat::expect_true(dynwrap::is_wrapper_with_expression(traj))

    expression <- traj$expression
    if (is.function(expression)) {
      expression <- expression()
    }
  }

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

  milenet_m <- milenet_m[,apply(milenet_m, 2, function(x) length(unique(x)) > 1)]
  importances <- map_df(seq_len(ncol(milenet_m)), function(i) {
    data <-
      milenet_m[,i, drop=F] %>%
      magrittr::set_colnames("PREDICT") %>%
      cbind(expression) %>%
      as.data.frame()

    importance <- get_importance(data, expression, method, method_params)

    data_frame(milestone_id = colnames(milenet_m)[[i]], feature_id = names(importance), importance)
  })

  importances %>%
    arrange(desc(importance))
}

#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or  \code{caret}. \code{calculate_overall_feature_importance} calculates the importance for the whole trajectory, \code{calculate_milestone_feature_importance} calculates it for individual milestones (eg. branching points)
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param expression The expression, if not provided will use the expression within the trajectory
#' @param method The method to do regressions, can be `ranger` or any regression model from caret
#' @param method_params Parameters given to the method
#' @param milestones_oi The milestone(s) for which to calculate feature importance
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
calculate_overall_feature_importance <- function(
  traj,
  expression = NULL,
  method = "ranger",
  method_params = list()
) {
  calculate_milestone_feature_importance(traj, expression, method=method, method_params=method_params) %>%
    group_by(feature_id) %>%
    summarise(importance=mean(importance)) %>%
    arrange(desc(importance))
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

    default_params <- list(form=PREDICT~., data=data, method=method)
    method_params <- list_modify(default_params, !!!method_params)

    if(!method %in% caret::modelLookup()$model) {stop("Invalid method")}

    model <- do.call(caret::train, method_params)
    caret::varImp(model)[[1]] %>% {set_names(.[, 1], rownames(.))}
  }
}
