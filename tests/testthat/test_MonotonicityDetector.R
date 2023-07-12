test_that("MonotonicityDetector on classif works", {
  # in the task below all features have no or a monotone increasing effect on the positive class label
  task = create_binary_classif_task(1000L)
  detector = MonotonicityDetector$new(task)

  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_equal(detector$rho_table[["rho"]], rep(0, length(task$feature_names)))
  detector$compute_rho_table()
  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_true(all(detector$rho_table[["rho"]] >= 0))
  expect_true(any(detector$rho_table[["rho"]] > 0))

  expect_equal(map_int(task$feature_names, function(feature_name) detector$get_sign(feature_name)), rep(1L, 6L))

  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_equal(detector$unconstrained_weight_table[["unconstrained_weight"]], rep(0, length(task$feature_names)))
  detector$compute_unconstrained_weight_table()
  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_true(all(detector$unconstrained_weight_table[["unconstrained_weight"]] <= 0.8) && all(detector$unconstrained_weight_table[["unconstrained_weight"]] >= 0.2))

  # in the task below all features have no or a monotone decreasing effect on the positive class label
  task = create_binary_classif_task(1000L)
  task$positive = "0"
  detector = MonotonicityDetector$new(task)

  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_equal(detector$rho_table[["rho"]], rep(0, length(task$feature_names)))
  detector$compute_rho_table()
  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_true(all(detector$rho_table[["rho"]] <= 0))
  expect_true(any(detector$rho_table[["rho"]] < 0))

  # no effect will result in a sign of 1
  expect_equal(map_int(task$feature_names[-6L], function(feature_name) detector$get_sign(feature_name)), rep(-1L, 5L))
  expect_equal(detector$get_sign("x6"), 1L)

  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_equal(detector$unconstrained_weight_table[["unconstrained_weight"]], rep(0, length(task$feature_names)))
  detector$compute_unconstrained_weight_table()
  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_true(all(detector$unconstrained_weight_table[["unconstrained_weight"]] <= 0.8) && all(detector$unconstrained_weight_table[["unconstrained_weight"]] >= 0.2))
})

test_that("MonotonicityDetectoron regr works", {
  # in the task below all features have no or a monotone increasing effect on y
  task = create_regr_task(1000L)
  detector = MonotonicityDetector$new(task)

  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_equal(detector$rho_table[["rho"]], rep(0, length(task$feature_names)))
  detector$compute_rho_table()
  expect_data_table(detector$rho_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$rho_table), c("feature_name", "rho"))
  expect_true(all(detector$rho_table[["rho"]] >= 0))
  expect_true(any(detector$rho_table[["rho"]] > 0))

  expect_equal(map_int(task$feature_names, function(feature_name) detector$get_sign(feature_name)), rep(1L, 6L))

  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_equal(detector$unconstrained_weight_table[["unconstrained_weight"]], rep(0, length(task$feature_names)))
  detector$compute_unconstrained_weight_table()
  expect_data_table(detector$unconstrained_weight_table, nrows = length(task$feature_names), ncols = 2L, col.names = "named")
  expect_equal(colnames(detector$unconstrained_weight_table), c("feature_name", "unconstrained_weight"))
  expect_true(all(detector$unconstrained_weight_table[["unconstrained_weight"]] <= 0.8) && all(detector$unconstrained_weight_table[["unconstrained_weight"]] >= 0.2))
})

