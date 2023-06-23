
# EA GGA

This is the official implemention of the EAGGA algorithm as introduced
in the paper ‘Multi-Objective Optimization of Performance and
Interpretability of Tabular Supervised Machine Learning Models’.

Toy example:

``` r
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(eagga)

task = tsk("spam")

resampling = rsmp("cv", folds = 3)

learner = as_learner(po("colapply") %>>% po("select") %>>% po("sortfeatures") %>>% lrn("classif.xgboost"))
learner$param_set$values$classif.xgboost.booster = "gbtree"
learner$param_set$values$classif.xgboost.tree_method = "exact"
learner$param_set$values$colapply.applicator = function(x) - x

measures = msrs(c("classif.ce", "selected_features_proxy", "selected_interactions_proxy", "selected_non_monotone_proxy"))

terminator = trm("evals", n_evals = 220)

search_space = ps(
  classif.xgboost.nrounds = p_dbl(lower = log(1), upper = log(5000), tags = c("int", "log"),
                                  trafo = function(x) as.integer(round(exp(x))), default = log(500)),
  classif.xgboost.eta = p_dbl(lower = log(1e-4), upper = log(1), tags = "log",
                              trafo = function(x) exp(x), default = log(0.3)),
  classif.xgboost.gamma = p_dbl(lower = log(1e-4), upper = log(7), tags = "log",
                                trafo = function(x) exp(x), default = log(1e-4)),
  classif.xgboost.lambda = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                 trafo = function(x) exp(x), default = log(1)),
  classif.xgboost.alpha = p_dbl(lower = log(1e-4), upper = log(1000), tags = "log",
                                trafo = function(x) exp(x), default = log(1e-4)),
  classif.xgboost.subsample = p_dbl(lower = 0.1, upper = 1, default = 1),
  classif.xgboost.max_depth = p_int(lower = 1, upper = 20, default = 6),
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
  mu = 100,
  lambda = 10,
  seed_calculate_proxy_measures = 1
)

tuner$optimize(instance)

instance$archive$best()

trained_learner = reconstruct_eagga_model(instance, tuner = tuner, model_uhash = instance$archive$best()$uhash[1])

groupstructure = instance$archive$best()$groupstructure[1][[1]]
groupstructure$selected_features
groupstructure$get_groups()
groupstructure$monotone_features

library(iml)
trained_learner$predict_type = "prob"
predictor = Predictor$new(trained_learner, data = task$data(cols = task$feature_names), y = task$data(cols = task$target_names)[[1]])
ale_unused = FeatureEffect$new(predictor, feature = groupstructure$unselected_features[1])
plot(ale_unused)
ale_used_mon = FeatureEffect$new(predictor, feature = groupstructure$monotone_features[monotonicity == 1]$feature[1])
plot(ale_used_mon)
ale_used_unc = FeatureEffect$new(predictor, feature = groupstructure$monotone_features[monotonicity == 0]$feature[1])
plot(ale_used_unc)
```

Notes:

- Check how to best store xgboost models etc.
- Maybe introduce a helper to construct a suitable GraphLearner
- Vignette for classif and regr example
