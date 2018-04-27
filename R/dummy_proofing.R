process_expression <- function(traj, expression) {
  # process expression
  if (is.null(expression)) {
    testthat::expect_true(dynwrap::is_wrapper_with_expression(traj))

    expression <- traj$expression
    if (is.function(expression)) {
      expression <- expression()
    }
  }
  expression
}