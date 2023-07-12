test_that("xgboost parsing classif", {
  task = create_binary_classif_task(1000L)
  learner = lrn("classif.xgboost", booster = "gbtree", tree_method = "exact")
  with_seed(2906, {
    learner$train(task) 
  })

  features = get_table_of_trees(learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))

  # one boosting round with one split
  learner$param_set$values$nrounds = 1L
  learner$param_set$values$max_depth = 1L
  with_seed(2906, {
    learner$train(task) 
  })
  features = get_table_of_trees(learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_equal(max(features$Tree), 0)
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  expect_equal(nrow(features[Feature == "Leaf", ]), 2L)
  expect_equal(length(unique(features$Feature)), 2L)  # Leaf and one feature
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, nrows = 0L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))

  # one boosting round with max_depth 2
  learner$param_set$values$nrounds = 1L
  learner$param_set$values$max_depth = 2L
  with_seed(2906, {
    learner$train(task) 
  })
  features = get_table_of_trees(learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_equal(max(features$Tree), 0)
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  expect_equal(nrow(features[Feature == "Leaf", ]), 4L)
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))
})

test_that("xgboost parsing regr", {
  task = create_regr_task(1000L)
  learner = lrn("regr.xgboost", booster = "gbtree", tree_method = "exact")

  with_seed(2906, {
    learner$train(task) 
  })
  features = xgb_model_dt_tree(model = learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))

  # one boosting round with one split
  learner$param_set$values$nrounds = 1L
  learner$param_set$values$max_depth = 1L
  with_seed(2906, {
    learner$train(task) 
  })
  features = get_table_of_trees(learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_equal(max(features$Tree), 0)
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  expect_equal(nrow(features[Feature == "Leaf", ]), 2L)
  expect_equal(length(unique(features$Feature)), 2L)  # Leaf and one feature
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, nrows = 0L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))

  # one boosting round with max_depth 2
  learner$param_set$values$nrounds = 1L
  learner$param_set$values$max_depth = 2L
  with_seed(2906, {
    learner$train(task) 
  })
  features = get_table_of_trees(learner$model)
  expect_data_table(features, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(features), c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover"))
  expect_equal(max(features$Tree), 0)
  expect_subset(unique(features$Feature), c("Leaf", task$feature_names))
  expect_equal(nrow(features[Feature == "Leaf", ]), 4L)
  pairs = get_interactions(learner$model)
  expect_data_table(pairs, min.rows = 1L, col.names = "named")
  expect_setequal(colnames(pairs), c("Parent", "Child", "Sum_gain", "Frequency"))
})

