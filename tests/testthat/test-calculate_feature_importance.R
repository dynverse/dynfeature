context("Testing calculate_feature_importance")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

num_features <- round(runif(1, 100, 120))
feature_names <- paste0("feature_", seq_len(num_features))

expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
counts <- round(2^expression - 1)


milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "man", "in", 1, TRUE,
  "in", "possession", 2, TRUE,
  "in", "of", 3, TRUE,
  "possession", "good", 4, TRUE,
  "of", "fortune", 5, TRUE,
  "good", "must", 6, TRUE,
  "fortune", "must", 7, TRUE
)
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "truth", "man", .8,
  "truth", "in", .2,
  "universally", "in", .3,
  "universally", "possession", .2,
  "universally", "of", .5,
  "acknowledged", "possession", 0,
  "acknowledged", "good", 1,
  "that", "good", .5,
  "that", "must", .5,
  "a", "good", .9,
  "a", "must", .1,
  "single", "fortune", .6,
  "single", "must", .4
)

divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "be", "in", TRUE,
  "be", "possession", FALSE,
  "be", "of", FALSE
)

wr <-
  dynwrap::wrap_data(
    id = id,
    cell_ids = cell_ids
  ) %>%
  dynwrap::add_expression(
    counts = counts,
    expression = expression
  ) %>%
  dynwrap::add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    milestone_percentages = milestone_percentages
  ) %>%
  dynwrap::add_waypoints()

method <- "ranger"

test_that("Testing calculate_overall_feature_importance", {
  gimp <- calculate_overall_feature_importance(wr, method=method)

  expect_equal(gimp %>% map_chr(class), c("feature_id" = "character", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% feature_names))
})

test_that("Testing calculate_milestone_feature_importance", {
  gimp <- calculate_milestone_feature_importance(wr, method=method, milestones_oi = milestone_ids)

  expect_equal(gimp %>% map_chr(class), c("milestone_id" = "character", "feature_id" = "character", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% feature_names))
})

test_that("Testing calculate_waypoint_feature_importance", {
  gimp <- calculate_waypoint_feature_importance(wr, method=method)

  expect_equal(gimp %>% map_chr(class), c("waypoint_id" = "character", "feature_id" = "character", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% feature_names))
})

test_that("Testing calculate_cell_feature_importance", {
  gimp <- calculate_cell_feature_importance(wr, method=method)

  expect_equal(gimp %>% map_chr(class), c("cell_id" = "character", "feature_id" = "character", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% feature_names))
})