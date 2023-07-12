test_that("TunerEAGGA on classif works", {
  # FIXME: also test sugar? tune, to_tune tokens etc.?
  task = create_binary_classif_task(1000L)

  resampling = rsmp("holdout")

  learner = as_learner(po("colapply") %>>% po("select") %>>% po("sortfeatures") %>>% lrn("classif.xgboost"))
  learner$param_set$values$classif.xgboost.booster = "gbtree"
  learner$param_set$values$classif.xgboost.tree_method = "exact"
  learner$param_set$values$colapply.applicator = function(x) - x

  measures = msrs(c("classif.ce", "selected_features_proxy", "selected_interactions_proxy", "selected_non_monotone_proxy"))

  terminator = trm("evals", n_evals = 20L)

  search_space = ps(
    classif.xgboost.nrounds = p_dbl(lower = log(1L), upper = log(500L), tags = c("int", "log"),
                                    trafo = function(x) as.integer(round(exp(x))), default = log(50L)),
    classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = log(1), tags = "log",
                                trafo = function(x) exp(x), default = log(0.3)),
    classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                   trafo = function(x) exp(x), default = log(1)),
    classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                  trafo = function(x) exp(x), default = log(1e-4)),
    classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
    classif.xgboost.max_depth = p_int(lower = 1L, upper = 20L, default = 6L),
    classif.xgboost.min_child_weight = p_dbl(lower = log(1), upper = log(150), tags = "log",
                                             trafo = function(x) exp(x), default = log(exp(1))),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, default = 1),
    classif.xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, default = 1),
    select.selector = p_uty(),  # must be part of the search space
    classif.xgboost.interaction_constraints = p_uty(),  # must be part of the search space
    classif.xgboost.monotone_constraints = p_uty()  # must be part of the search space
  )

  instance = TuningInstanceMultiCrit$new(
    task = task,
    learner = learner,
    resampling = resampling, 
    measures = measures,
    terminator = terminator,
    search_space = search_space,
    store_models = TRUE
  )

  tuner = tnr("eagga",
    learner_id = "classif.xgboost",
    select_id = "select.selector",
    interaction_id = "classif.xgboost.interaction_constraints",
    monotone_id = "classif.xgboost.monotone_constraints",
    mu = 10L,
    lambda = 2L,
    seed_calculate_proxy_measures = 1
  )

  expect_setequal(tuner$param_set$ids(), c("learner_id", "select_id", "interaction_id", "monotone_id", "mu", "lambda", "seed_calculate_proxy_measures"))

  tuner$optimize(instance)
  # FIXME: Error: Tables have different number of rows (x: 20, y: 40)
  expect_true(instance$is_terminated)
  expect_true(nrow(instance$archive$data) == 20L)
  expect_true(all(c(tuner$param_set$values$select_id, tuner$param_set$values$interaction_id, tuner$param_set$values$monotone_id,
                    "groupstructure", "groupstructure_orig", "generation", "status") %in% colnames(instance$archive$data)))
  expect_list(instance$archive$data[[tuner$param_set$values$select_id]], types = "Selector")
  expect_true(all(c("n_selected", "n_selected_total") %in% names(attributes(instance$archive$data[[tuner$param_set$values$select_id]][[1L]]))))
  expect_list(instance$archive$data[[tuner$param_set$values$interaction_id]], types = "list")
  expect_list(instance$archive$data[[tuner$param_set$values$interaction_id]][[1L]], types = "integer")
  expect_true(all(c("n_interactions", "n_interactions_total") %in% names(attributes(instance$archive$data[[tuner$param_set$values$interaction_id]][[1L]]))))
  expect_list(instance$archive$data[[tuner$param_set$values$monotone_id]], types = "integer")
  expect_true(!is.null(names(instance$archive$data[[tuner$param_set$values$monotone_id]][[1L]])))
  expect_true(all(c("n_non_monotone", "n_non_monotone_total") %in% names(attributes(instance$archive$data[[tuner$param_set$values$monotone_id]][[1L]]))))
  expect_list(instance$archive$data$groupstructure, types = "GroupStructure")
  expect_list(instance$archive$data$groupstructure_orig, types = "GroupStructure")
  expect_integerish(instance$archive$data$generation)
  expect_true(max(instance$archive$data$generation) == 5L)
  expect_true(all(instance$archive$data$status %in% c("alive", "dead")))
  expect_true(sum(instance$archive$data$status == "alive") == 10L)

  # optimization is useful
  expect_true(any(instance$archive$best()[["classif.ce"]] < 0.5))
  expect_true(any(instance$archive$best()[["selected_features_proxy"]] < 1))
  expect_true(any(instance$archive$best()[["selected_interactions_proxy"]] < 1))
  expect_true(any(instance$archive$best()[["selected_non_monotone_proxy"]] < 1))

  expect_true(var(instance$archive$best()[["classif.ce"]]) > 0)
  expect_true(var(instance$archive$best()[["selected_features_proxy"]]) > 0)
  expect_true(var(instance$archive$best()[["selected_interactions_proxy"]]) > 0)
  expect_true(var(instance$archive$best()[["selected_non_monotone_proxy"]]) > 0)

  # FIXME:
  expect_class(instance$objective$learner$param_set$values[["colapply.affect_columns"]], classes = "Selector")
 
  # also test: 
  #private$.n_selected_prob
  #private$.n_selected
  #private$.filter
  #private$.scores
  #private$.interaction_detector
  #private$.monotonicity_detector
  #private$.unconstrained_weight_table
  #private$.switch_sign_affected

  # FIXME: more tests about same group structure and proxy measures
  expect_error(reconstruct_eagga_model(instance, tuner = tuner, model_uhash = "test"), "Assertion on 'model_uhash' failed")
  trained_learner = reconstruct_eagga_model(instance, tuner = tuner, model_uhash = instance$archive$best()$uhash[1L])
  expect_learner(trained_learner, task = task)
  trained_learner$predict_type = "prob"
  predictions = trained_learner$predict(task)
  expect_prediction(predictions)
  expect_numeric(predictions$score(measures), lower = 0, upper = 1)

  # FIXME: also test regr works
})
