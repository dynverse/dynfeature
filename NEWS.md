# dynfeature 1.0.1

* MINOR CHANGE: Fixes to the description.

# dynfeature 1.0.0

Initial release of dynfeature on CRAN.

* MINOR CHANGE: Use only one core by default.

* MINOR CHANGE: Support sparse matrices.

* DOCUMENTATION: Add examples and returns.

# dynfeature 0.2.0 (25-10-2018)

* SPEED UP: Added `fi_ranger_rf_lite()`, which scales much better w.r.t. the number of samples and features, 
  at the cost of increasing loss of accuracy at higher dimension sizes.
  
* MAJOR CHANGES: Large cleanup of the code. Most notably,
  - The format of feature importance method specification and its parameters, 
    with format `fi_method = fi_example_method(param1 = 10, param2 = 4)`. 
    Before, it had to be specified as `method = "example_method", method_params = list(param1 = 10, param2 = 4)`.

* MINOR CHANGE: Whenever possible, output columns are now factors instead of characters.

* MINOR CHANGE: Add NEWS, and add news section to README.

* DOCUMENTATION: Turned on markdown for Roxygen.

* DOCUMENTATION: Improved documentation on expression_source.

* TESTING: Improved testing with a larger dataset, and will check whether the overall feature importance produces decent results.

* MINOR CHANGE: Feature importance functions will always return factors instead of characters.

# dynfeature 0.1.0 (26-04-2018)

* INITIAL RELEASE: dynfeature, calculating feature importance scores from trajectories.