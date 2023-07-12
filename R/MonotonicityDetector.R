#' @title Monotonicity Detector
#'
#' @description
#' Detects whether a feature should have a monotone increasing or decreasing effect (if enforced) and quantifies
#' the tendency of a monotone relationship between the feature and the target.
#'
#' This detector utilizes decision trees to fit the feature and target variable using subsampled data and calculates
#' Spearman's Rho between the predicted values (numeric for regression and probability of the positive class for
#' binary classification) and the feature.
#' The Spearman's Rho values serve as indicators of the presence of a monotone relationship between features and the target
#' and can also help infer the sign of the feature if it should be constrained to have a monotone effect.
#'
#' Note that this detector only works with integer or numeric features.
#' Logical features must be converted to integers.
#'
#' @export
MonotonicityDetector = R6Class("MonotonicityDetectorDetector",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task)\cr
    #'   The task.
    initialize = function(task) {
      assert_task(task, feature_types = c("integer", "numeric"))
      feature_names = task$feature_names
      feature_types = task$feature_types
      y = task$data(cols = task$target_names)[[1L]]
      n_features = length(feature_names)
      self$task = task
      self$n_features = n_features
      self$feature_names = feature_names
      self$feature_types = feature_types
      self$rho_table = data.table(feature_name = feature_names, rho = numeric(n_features))
      self$unconstrained_weight_table = data.table(feature_name = feature_names, unconstrained_weight = numeric(n_features))
    },

    #' @field task ([mlr3::Task])\cr
    #'   The task.
    task = NULL,
    
    #' @field n_features (integer(1))\cr
    #'   The number of features in the [mlr3::Task].
    n_features = NULL,
    
    #' @field feature_names (character()) \cr
    #'   The names of the features in the [mlr3::Task].
    feature_names = NULL,
    
    #' @field feature_types (character()) \cr
    #'   The types of the features in the [mlr3::Task].
    feature_types = NULL,
    
    #' @field rho_table ([data.table::data.table])\cr
    #'   A data.table with as many rows as features.
    #'   It has a "feature_name" column indicating the feature and a "rho" column indicating Spearman's Rho.
    rho_table = NULL,
    
    #' @field unconstrained_weight_table ([data.table::data.table])\cr
    #'   A data.table with as many rows as features.
    #'   It has a "feature_name" column indicating the feature and an "unconstrained_weight" column indicating 1 minus the absolute
    #'   Spearman's Rho rescaled to the range `[0.2, 0.8]`.
    unconstrained_weight_table = NULL,

    #' @description
    #'
    #' Fits a decision tree for each feature and the target variable using subsampled data and calculates Spearman's Rho
    #' between the predicted values (numeric for regression and probability of the positive class for binary classification)
    #' and the feature. This process is repeated for each feature ten times, and the average Spearman's Rho values are stored
    #' in the `$rho_table`. The subsampling fraction is set to `0.9`, and the decision tree used is a standard
    #' [mlr3::LearnerRegr] or [mlr3::LearnerClassif].
    #'
    #' @return Invisible (NULL)
    compute_rho_table = function() {
      # NOTE: expose hyperparameters?
      pb = progress_bar$new(format = "Detecting monotonicity [:bar] :percent eta: :eta", total = length(self$feature_names))
      for (feature_name in self$feature_names) {
        pb$tick()
        private$.compute_rho(feature_name)
      }
      self$rho_table[is.na(rho), rho := 0]

      invisible(NULL)
    },

    #' @description
    #'
    #' Determines the sign of a given feature based on the average Spearman's Rho correlation, if the feature should be
    #' constrained to have a monotone effect.
    #'
    #' @param feature_name (character(1))\cr
    #'   The feature for which the sign should be determined.
    #'
    #' @return
    #' An integer vector indicating the sign of the feature if it should be constrained to have a monotone effect.
    get_sign = function(feature_name) {
      feature_name_ = assert_choice(feature_name, choices = self$feature_names)
      feature_sign = as.integer(sign(self$rho_table[feature_name == feature_name_, rho]))
      if (feature_sign == 0) {
        feature_sign = 1
      }
      as.integer(feature_sign)
    },

    #' @description
    #'
    #' Constructs the unconstrained weights table from the average Spearman's Rhos by taking 1 minus the absolute value and scaling it to the range `[0.2, 0.8]`.
    #'
    #' @return Invisible (NULL)
    compute_unconstrained_weight_table = function() {
      self$unconstrained_weight_table = self$rho_table
      colnames(self$unconstrained_weight_table) = c("feature_name", "unconstrained_weight")
      self$unconstrained_weight_table[, unconstrained_weight := 1 - abs(unconstrained_weight)]
      self$unconstrained_weight_table[, unconstrained_weight := ((unconstrained_weight - 0) / (1 - 0)) * (0.8 - 0.2) +  0.2]  # bound by [0.2, 0.8]

      invisible(NULL)
    }
  ),

  private = list(
    .compute_rho = function(feature_name, repls = 10L) {
    task = self$task$clone(deep = TRUE)
    if (task$task_type == "classif") {
      learner = as_learner(po("subsample", frac = 0.9) %>>% lrn("classif.rpart"))
      learner$predict_type = "prob"
    } else if (task$task_type == "regr") {
      learner = as_learner(po("subsample", frac = 0.9) %>>% lrn("regr.rpart"))
    }
    task$col_roles$feature = feature_name
    rho_repls = map_dbl(seq_len(repls), function(repl) {
      learner$train(task)
      pred = learner$predict(task)
      if (task$task_type == "classif") {
        y = pred$prob[, task$positive]
      } else if (task$task_type == "regr") {
        y = pred$response
      }
      if (sd(y) < sqrt(.Machine$double.eps) | sd(task$data(cols = feature_name)[[1L]]) < sqrt(.Machine$double.eps)) {
        0
      } else {
        cor(y, task$data(cols = feature_name)[[1L]], method = "spearman")
      }
    })
      self$rho_table[feature_name == feature_name, rho := mean(rho_repls)]
    }
  )
)

