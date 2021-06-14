context("Testing calculate_feature_importance")

# v this should be put in a separate helper... but it doesn't want to work...

milestone_ids <- c("A", "B", "C", "D")
milestone_network <- tibble::tribble(
  ~from, ~to, ~length, ~directed,
  "A",   "B", 1,       TRUE,
  "B",   "C", 1,       TRUE,
  "B",   "D", 1,       TRUE,
  "A",   "D", 1,       TRUE
)

num_cells <- 100
num_features <- 20

cell_ids <- paste0("cell_", seq_len(num_cells))
feature_ids <- paste0("feature_", seq_len(num_features))

# generate random cell positions
progressions <-
  milestone_network %>%
  select(from, to) %>%
  sample_n(num_cells, replace = TRUE) %>%
  mutate(
    cell_id = cell_ids,
    percentage = runif(n())
  ) %>%
  select(cell_id, from, to, percentage)

trajectory <-
  dynwrap::wrap_data(
    cell_ids = cell_ids
  ) %>%
  dynwrap::add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions
  )

# generate module positions
num_modules <- 10
module_ids <- paste0("module_", seq_len(num_modules))
module_progressions <-
  milestone_network %>%
  select(from, to) %>%
  sample_n(num_modules, replace = TRUE) %>%
  mutate(
    module_id = module_ids,
    percentage = runif(n()),
    basal = rnorm(n(), 10, 3) %>% pmax(0),
    mult = rnorm(n(), 5, 1) %>% pmax(.1),
    sd = rnorm(n(), .5, .1) %>% pmax(0)
  )

module_percentages <-
  dynwrap::convert_progressions_to_milestone_percentages(
    cell_ids = module_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = module_progressions %>% rename(cell_id = module_id)
  ) %>%
  rename(module_id = cell_id)

# generate module expression
distance_from_module <-
  dynwrap::calculate_geodesic_distances(
    trajectory,
    waypoint_milestone_percentages = module_percentages %>% rename(waypoint_id = module_id)
  )

suppressWarnings({
  module_expression <-
    distance_from_module %>%
    reshape2::melt(varnames = c("module_id", "cell_id"), value.name = "distance") %>%
    as_tibble(.name_repair = "minimal") %>%
    left_join(module_progressions %>% select(-from, -to, -percentage), by = "module_id") %>%
    mutate(
      expr = dnorm(distance, mean = 0, sd = sd) * mult + basal
    ) %>%
    reshape2::acast(module_id ~ cell_id, value.var = "expr")
})
module_expression <- module_expression[module_ids, cell_ids]

# generate gene expression
feature_info <- tibble(
  feature_id = feature_ids,
  module = sample.int(num_modules, num_features, replace = TRUE),
  mean = rnorm(num_features, 10, 3) %>% pmax(0),
  sd = rnorm(num_features, 3, 1) %>% pmax(.1),
  dropout = runif(num_features, 0, 1)
)

expression <-
  feature_info %>%
  group_by(feature_id) %>%
  purrr::pmap_df(function(feature_id, module, mean, sd, dropout) {
    tibble(
      cell_id = colnames(module_expression),
      feature_id,
      expression = ifelse(runif(num_cells) < dropout, 0, module_expression[module, , drop = FALSE] + rnorm(num_cells, mean, sd))
    )
  }) %>%
  reshape2::acast(cell_id ~ feature_id, value.var = "expression")

expression <- expression[cell_ids, feature_ids]

counts <- round(2^expression - 1)

# add expression to trajectory
trajectory <-
  trajectory %>%
  dynwrap::add_expression(
    counts = counts,
    expression = expression,
    feature_info = feature_info
  ) %>%
  dynwrap::add_waypoints()


# ^ this should be put in a separate helper... but it doesn't want to work...





test_that("Testing calculate_overall_feature_importance", {
  gimp <- calculate_overall_feature_importance(trajectory)

  expect_equal(gimp %>% map_chr(class), c("feature_id" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% trajectory$feature_info$feature_id))

  # there should be an inverse correlation between the noise parameters in the generated data, and the importance values
  suppressWarnings({
    join <- trajectory$feature_info %>% inner_join(gimp, by = "feature_id")
  })

  expect_lte(with(join, cor(sd * dropout, importance)), -.1)
})

test_that("Testing calculate_milestone_feature_importance", {
  gimp <- calculate_milestone_feature_importance(trajectory, milestones_oi = trajectory$milestone_ids)

  expect_equal(gimp %>% map_chr(class), c("milestone_id" = "factor", "feature_id" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% trajectory$feature_info$feature_id))
})

test_that("Testing calculate_waypoint_feature_importance", {
  gimp <- calculate_waypoint_feature_importance(trajectory)

  expect_equal(gimp %>% map_chr(class), c("waypoint_id" = "factor", "feature_id" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% trajectory$feature_info$feature_id))
})

test_that("Testing calculate_cell_feature_importance", {
  gimp <- calculate_cell_feature_importance(trajectory)

  expect_equal(gimp %>% map_chr(class), c("cell_id" = "factor", "feature_id" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% trajectory$feature_info$feature_id))
})

test_that("Testing calculate_branching_point_feature_importance", {
  suppressWarnings({
    gimp <- calculate_branching_point_feature_importance(trajectory)
  })
  expect_equal(gimp %>% map_chr(class), c("milestone_id" = "factor", "feature_id" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% trajectory$feature_info$feature_id))
})

test_that("Testing calculate_branch_feature_importance", {
  gimp <- calculate_branch_feature_importance(trajectory)

  expect_equal(gimp %>% map_chr(class), c("feature_id" = "factor", "from" = "factor", "to" = "factor", "importance" = "numeric"))

  expect_true(all(unique(gimp$feature_id) %in% feature_ids))
})

