
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

measures = list(msr("classif.ce"),
                msr("selected_features_proxy"),
                msr("selected_interactions_proxy"),
                msr("selected_non_monotone_proxy"))

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

tuner = tnr("eagga", mu = 20, lambda = 10)
tuner$optimize(instance)
```

Notes:
if we want to perfectly reproduce the fitted models during optimization:
(i) we have to make the learning algorithm deterministic e.g., set.seed() passed to calculate_proxy_measures is
sufficient for xgboost
(ii) we have to use the learner of the instance which during optimization is updated with which signs to flip
learner_for_measures = inst$objective$learner$clone(deep = TRUE)
orig_pvs = learner_for_measures$param_set$values
FIXME: we should write this somewhere else also
(iii) we need to create selector, interaction and monotonicity based on the orig groupstructure of the archive and not
the logged columns or the groupstructure which are the post hoc updated group structure after having evlauated it and
using the potentially more restrictive updated group structure might result in an even smaller model when refitting
i.e.
select_id = "select.selector"
interaction_id = "classif.xgboost.interaction_constraints"
monotone_id = "classif.xgboost.monotone_constraints"
xy = instance$archive$best()[12L, ]
xdt = xy[, instance$archive$cols_x, with = FALSE]
xdt1 = copy(xdt)
groupstructure = xy$groupstructure_orig[[1L]]
xdt[1, ][[select_id]][[1L]] = groupstructure$create_selector()
xdt[1, ][[interaction_id]][[1L]] = groupstructure$create_interaction_constraints()
xdt[1, ][[monotone_id]][[1L]] = groupstructure$create_monotonicity_constraints()
(iv) I should check how to best store xgboost models etc
(v) state how iml can be used on such a model

maybe add reproduce model from archive function
