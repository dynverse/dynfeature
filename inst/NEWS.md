# dynfeature 0.2.0 (21-10-2018)

* SPEED UP: Added `fi_ranger_rf_lite()`, which scales much better w.r.t. the number of samples and features, 
  at the cost of increasing loss of accuracy at higher dimension sizes.
  
* MAJOR CHANGES: Large cleanup of the code. 

* MINOR CHANGE: Whenever possible, output columns are now factors instead of characters.

* MINOR CHANGE: Add NEWS, and add news section to README.

# dynfeature 0.1.0 (26-04-2018)

Initial release of dynfeature, which provides common functionality for calculating feature importance scores from trajectories.