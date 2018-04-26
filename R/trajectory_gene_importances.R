#' Calculating feature importances across trajectories
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}} or  \code{caret}.
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param method The method to do regressions, can be `ranger` or any regression model from caret
#' @param method_params Parameters given to the method
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
trajectory_feature_importances <- function(
  traj,
  method = "ranger",
  method_params = list()
) {
  testthat::expect_true(dynwrap::is_wrapper_with_expression(traj))
  testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))

  cell_ids <- traj$cell_ids
  expression <- traj$expression

  if (is.function(expression)) {
    expression <- expression()
  }

  milenet_m <- traj$milestone_percentages %>%
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

  importances <- importances %>%
    group_by(feature_id) %>%
    summarise(importance=mean(importance)) %>%
    arrange(desc(importance))

  importances
}


get_importance <- function(data, expression, method, method_params) {
  if (method == "ranger") {
    requireNamespace("ranger")
    # process ranger params

    default_params <- list(dependent.variable.name="PREDICT",data = data, importance="impurity")
    method_params <- list_modify(default_params, !!!method_params)
    if ("mtry" %in% names(method_params)) {method_params$mtry <- method_params$mtry(expression)}

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
