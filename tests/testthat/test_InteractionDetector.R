test_that("InteractionDetector on classif works", {
  # in the task below x1 and x2 interact and x3 and x4
  task = create_binary_classif_task(1000L)
  detector = InteractionDetector$new(task, grid_size = 11L)
  expect_equal(detector$rss, NULL)
  detector$compute_best_rss()
  expect_matrix(detector$rss, any.missing = FALSE, nrows = length(task$feature_names), ncols = length(task$feature_names))
  expect_true(all(detector$rss <= 0))  # reduction in RSS
  best_1 = detector$get_eqcs_from_top_k(1)
  expect_equal(best_1, setNames(c(1, 1, 2, 3, 4, 5), nm = c("x1", "x2", "x3", "x4", "x5", "x6")))
  best_1_second = detector$get_eqcs_from_top_k(1, features = c("x3", "x4", "x5", "x6"))
  expect_equal(best_1_second, setNames(c(1, 1, 2, 3), nm = c("x3", "x4", "x5", "x6")))
})

test_that("InteractionDetector on regr works", {
  # in the task below x1 and x2 interact and x3 and x4
  task = create_regr_task(1000L)
  detector = InteractionDetector$new(task, grid_size = 11L)
  expect_equal(detector$rss, NULL)
  detector$compute_best_rss()
  expect_matrix(detector$rss, any.missing = FALSE, nrows = length(task$feature_names), ncols = length(task$feature_names))
  expect_true(all(detector$rss <= 0))  # reduction in RSS
  best_1 = detector$get_eqcs_from_top_k(1)
  expect_equal(best_1, setNames(c(1, 1, 2, 3, 4, 5), nm = c("x1", "x2", "x3", "x4", "x5", "x6")))
  best_1_second = detector$get_eqcs_from_top_k(1, features = c("x3", "x4", "x5", "x6"))
  expect_equal(best_1_second, setNames(c(1, 1, 2, 3), nm = c("x3", "x4", "x5", "x6")))
})

