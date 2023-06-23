#' @title Monotonicity Detector 
#'
#' @description
#' Detects whether a feature should have a monotone increasing or decreasing effect (if enforced) and also quantifies
#' the tendency of a monotone relationship between the feature and the target.
#'
#' Only works with integer or numeric features.
#' Logical features must be converted to integers.
#'
#' @export
MonotonicityDetector = R6Class("MonotonicityDetectorDetector",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task)\cr
    #'   Task.
    initialize = function(task) {
      assert_task(task, feature_types = c("integer", "numeric"))
      feature_names = task$feature_names
      feature_types = task$feature_types
      y = task$data(cols = task$target_names)[[1L]]  # regardless of regr or classif
      n_features = length(feature_names)
      self$task = task
      self$classification = task$task_type == "classif"
      self$data = task$data()
      self$n_features = n_features
      self$feature_names = feature_names
      self$feature_types = feature_types
      self$y_name = task$target_names
      self$rho_table = data.table(feature_name = feature_names, rho = numeric(n_features))
      self$unconstrained_weight_table = data.table(feature_name = feature_names, unconstrained_weight = numeric(n_features))
    },

    #' @field task ([mlr3::Task])\cr
    #'   Task.
    task = NULL,

    #' @field classification (logical(1))\cr
    #'   Whether the task is a classification task.
    classification = NULL,

    #' @field data ([data.table::data.table])\cr
    #'   data.table of data of the [mlr3::Task].
    data = NULL,

    #' @field n_features (integer(1))\cr
    #'   Number of features part of the [mlr3::Task].
    n_features = NULL,
    
    #' @field feature_names (character()) \cr
    #'   The names of the features of the [mlr3::Task].
    feature_names = NULL,

    #' @field feature_types (character()) \cr
    #'   The types of the features of the [mlr3::Task].
    feature_types = NULL,

    #' @field y_name (character(1)) \cr
    #'   The names of the target of the [mlr3::Task].
    y_name = NULL,

    #' @field rho_table ([data.table::data.table])\cr
    #'   data.table with as many rows as features with the feature_name column indicating the feature and the rho column
    #'   indicating Spearman's Rho.
    rho_table = NULL,
    
    #' @field unconstrained_weight_table ([data.table::data.table])\cr
    #'   data.table with as many rows as features with the feature_name column indicating the feature and the unconstrained_weight column
    #'   indicating the absolute Spearman's Rho scaled rescaled to `[0.2, 0.8]`.
    unconstrained_weight_table = NULL,

    #' @description
    #'
    #' Fits a decision tree for each feature and the target variable using subsampled data and calculates Spearman's Rho between the prediction (numeric for regression and probability of the
    #' positive class for binary classification) and the feature.
    #' Repeats this process for each feature ten times and averages the Spearman's Rhos to fill the `$rho_table`
    #' Subsampling fraction is set to `0.9` and the decision tree is a standard [mlr3::LearnerRegr] or [mlr3::LearnerClassif].
    #' FIXME: expose hyperparameters etc.
    #'
    #' @return Invisible (`NULL`)
    compute = function() {
      pb = progress_bar$new(format = "Detecting monotonicity [:bar] :percent eta: :eta", total = length(self$feature_names))
      for (x_name in self$feature_names) {
        pb$tick()
        private$.compute_rho(x_name)
      }
      self$rho_table[is.na(rho), rho := 0]

      invisible(NULL)
    },


    #' @description
    #'
    #' Method to determine the sign of a given feature based on the average Spearman's Rho correlation if it should be constrained to have a monotone effect.
    #' @param feature_name (`character(1)`)\cr
    #'  Feature for which the sign should be determined
    #'
    #' @return (`integer()`) vector indicating the sign of the feature if it should be constrained to have a monotone effect
    get_sign = function(feature_name) {
      x_name = assert_choice(feature_name, choices = self$feature_names)
      as.integer(sign(self$rho_table[feature_name == x_name, rho]))
    },

    #' @description
    #'
    #' Method to construct the unconstrained weights table from the average Spearman's Rhos by taking the absolute value
    #' and scaling it to `[0.2, 0.8]`.
    #'
    #' @return Invisible (`NULL`)
    compute_unconstrained_weights = function() {
      self$unconstrained_weight_table = self$rho_table
      colnames(self$unconstrained_weight_table) = c("feature_name", "unconstrained_weight")
      self$unconstrained_weight_table[, unconstrained_weight := 1 - abs(unconstrained_weight)]
      self$unconstrained_weight_table[, unconstrained_weight := ((unconstrained_weight - 0) / (1 - 0)) * (0.8 - 0.2) +  0.2]  # bound by [0.2, 0.8]

      invisible(NULL)
    }
  ),

  private = list(
    .compute_rho = function(x_name, repls = 10L) {
    # FIXME: return 0 if sd is 0
    task = self$task$clone(deep = TRUE)
    if (task$task_type == "classif") {
      learner = as_learner(po("subsample", frac = 0.9) %>>% lrn("classif.rpart"))
      learner$predict_type = "prob"
    } else if (task$task_type == "regr") {
      learner = as_learner(po("subsample", frac = 0.9) %>>% lrn("regr.rpart"))
    }
    task$col_roles$feature = x_name
    rho_repls = map_dbl(seq_len(repls), function(repl) {
      learner$train(task)
      pred = learner$predict(task)
      if (task$task_type == "classif") {
        y = pred$prob[, 1L]
      } else if (task$task_type == "regr") {
        y = pred$response
      }
      cor(y, task$data(cols = x_name)[[1L]], method = "spearman")
    })

      self$rho_table[feature_name == x_name, rho := mean(rho_repls)]
    }
  )
)

### test
if (FALSE) {
  task = tsk("spam")
  detector = MonotonicityDetector$new(task)
  detector$compute()
  detector$rho_table
  detector$get_sign("address")
  detector$compute_unconstrained_weights()
  detector$unconstrained_weight_table
}
