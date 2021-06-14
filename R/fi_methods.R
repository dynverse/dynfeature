apply_function_params <- function(params, nrow, ncol) {
  map(params, function(param) {
    if (is.function(param) && length(methods::formalArgs(param)) == 2 && all(c("nrow", "ncol") %in% methods::formalArgs(param))) {
      param(nrow = nrow, ncol = ncol)
    } else {
      param
    }
  })
}

#' Feature Importance methods
#'
#' @param num_trees (fi_ranger_rf_lite) The number of trees to use
#' @param num_variables_per_split (fi_ranger_rf_lite) The number of variables to sample per split
#' @param num_samples_per_tree (fi_ranger_rf_lite) The number of samples to bootstrap per split
#' @param min_node_size (fi_ranger_rf_lite) The minimum node size, no split will be made if the node size is less than this value.
#' @param ... Extra parameters to pass onto the underlying feature importance function.
#'
#' @returns A list containing a helper function for calling a feature importance function.
#'
#' @rdname fi_methods
#'
#' @export
#'
#' @examples
#' library(dynwrap)
#' data(example_trajectory)
#'
#' calculate_overall_feature_importance(example_trajectory, fi_method = fi_ranger_rf())
fi_ranger_rf_lite <- function(
  num_trees = 2000,
  num_variables_per_split = 50,
  num_samples_per_tree = 250,
  min_node_size = 20,
  ...
) {
  fi_ranger_rf(
    num.trees = num_trees,
    mtry = function(nrow, ncol) min(num_variables_per_split, ncol),
    sample.fraction = function(nrow, ncol) min(num_samples_per_tree / nrow, 1),
    min.node.size = min_node_size,
    ...
  )
}

#' @rdname fi_methods
#'
#' @export
fi_ranger_rf <- function(
  ...
) {
  params <- lst(...)
  list(
    fun = function(X, y, verbose) {
      requireNamespace("ranger")

      data <- data.frame(
        PREDICT = y,
        X,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      default_params <- list(
        num.threads = 1,
        importance = "impurity",
        mtry = function(nrow, ncol) max(sqrt(ncol) * .01, 1),
        write.forest = FALSE
      )

      method_params <-
        default_params %>%
        list_modify(!!!params) %>%
        apply_function_params(nrow = nrow(X), ncol = ncol(X)) %>%
        list_modify(
          data = data,
          dependent.variable.name = "PREDICT",
          verbose = verbose
        )

      do.call(ranger::ranger, method_params)$variable.importance
    }
  )
}

#' @param caret_method (fi_caret) Which caret method to use for feature importance.
#'
#' @rdname fi_methods
#'
#' @export
fi_caret <- function(
  caret_method,
  ...
) {
  requireNamespace("caret")

  if (!caret_method %in% caret::modelLookup()$model) stop("Invalid method")

  params <- lst(method = caret_method, ...)

  list(
    fun = function(X, y, verbose) {
      requireNamespace("caret")

      data <- data.frame(
        PREDICT = y,
        X,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      default_params <- list(
        trControl = caret::trainControl(method = "none")
      )

      method_params <-
        default_params %>%
        list_modify(!!!params) %>%
        apply_function_params(nrow = nrow(X), ncol = ncol(X)) %>%
        list_modify(
          form = PREDICT ~ .,
          data = data
        )

      model <- do.call(caret::train, method_params)

      vi <- caret::varImp(model)[[1]]

      set_names(vi[, 1], rownames(vi))
    }
  )
}



#' @rdname fi_methods
#'
#' @export
fi_ranger_rf_tiny <- function(
  num_trees = 100,
  num_variables_per_split = 50,
  num_samples_per_tree = 250,
  min_node_size = 20,
  ...
) {
  fi_ranger_rf(
    num.trees = num_trees,
    mtry = function(nrow, ncol) min(num_variables_per_split, ncol),
    sample.fraction = function(nrow, ncol) min(num_samples_per_tree / nrow, 1),
    min.node.size = min_node_size,
    ...
  )
}