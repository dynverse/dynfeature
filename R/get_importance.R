get_importance <- function(data, expression, method, method_params, verbose = FALSE) {
  if (method == "ranger") {
    requireNamespace("ranger")
    # process ranger params

    default_params <- list(
      dependent.variable.name = "PREDICT",
      data = data,
      importance = "impurity",
      mtry = function(x) ncol(x) * .01,
      num.threads = 1,
      verbose = verbose
    )
    method_params <- list_modify(default_params, !!!method_params)
    if (is.function(method_params$mtry)) {method_params$mtry <- method_params$mtry(expression)}

    # call ranger and get variable importance
    do.call(ranger::ranger, method_params)$variable.importance
  } else {
    requireNamespace("caret")

    default_params <- list(form = PREDICT ~ ., data = data, method = method, trControl = trainControl(method = "none"))
    method_params <- list_modify(default_params, !!!method_params)

    if(!method %in% caret::modelLookup()$model) {stop("Invalid method")}

    model <- do.call(caret::train, method_params)
    caret::varImp(model)[[1]] %>% {set_names(.[, 1], rownames(.))}
  }
}