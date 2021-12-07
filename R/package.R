#' Feature Importance for Dynamic Processes
#'
#' Calculating feature importance scores from trajectories using the random forests algorithm.
#'
#' @import dplyr
#' @importFrom methods formalArgs
#' @importFrom tibble tibble tribble as_tibble
#' @importFrom dynutils expand_matrix is_sparse
#' @importFrom dynwrap get_expression is_wrapper_with_waypoints is_wrapper_with_trajectory add_waypoints
#' @importFrom purrr %>% map map_df map_chr map_lgl map_int map_dbl keep set_names list_modify invoke
#' @importFrom reshape2 acast melt
#'
#' @docType package
#' @name dynfeature
NULL
