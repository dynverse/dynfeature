
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynfeature

**dynfeature** provides common functionality for calculating feature
importance scores from trajectories

[![Build
Status](https://travis-ci.org/dynverse/dynfeature.svg)](https://travis-ci.org/dynverse/dynfeature)
[![codecov](https://codecov.io/gh/dynverse/dynfeature/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynfeature)

Included are methods to

  - Calculate the overall feature importance, using
    `calculate_overall_feature_importance`
  - Calculate the importance of a feature at a bifurcation point, using
    `calculate_milestone_feature_importance`

The plotting of the top features is nicely intergrated into
[dynplot](https://github.com/dynverse/dynplot)

[![dynplot
heatmap](https://raw.githubusercontent.com/dynverse/dynplot/devel/.readme_files/heatmap-1.png)](https://github.com/dynverse/dynplot)

## Latest changes

Check out `news(package = "princurve")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Latest changes in dynfeature 0.2.0 (21-10-2018)

  - SPEED UP: Added `fi_ranger_rf_lite()`, which scales much better
    w.r.t. the number of samples and features, at the cost of increasing
    loss of accuracy at higher dimension sizes.

  - MAJOR CHANGES: Large cleanup of the code. Most notably,
    
      - The format of feature importance method specification and its
        parameters, with format `fi_method = fi_example_method(param1
        = 10, param2 = 4)`. Before, it had to be specified as `method =
        "example_method", method_params = list(param1 = 10, param2
        = 4)`.

  - MINOR CHANGE: Whenever possible, output columns are now factors
    instead of characters.

  - MINOR CHANGE: Add NEWS, and add news section to README.

### Latest changes in dynfeature 0.1.0 (26-04-2018)

  - INITIAL RELEASE: dynfeature, calculating feature importance scores
    from trajectories.
