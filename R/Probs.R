#' @title Probs
#'
#' @description
#' Class to represent probabilities used in mutation and crossover operations.
#'
#' @details
#' This class provides a convenient way to manage and update probabilities used in mutation and crossover operations.
#' It allows you to set individual probabilities for various operations, such as overall mutation, overall crossover,
#' parameter mutation, parameter crossover, and group structure mutation.
#' Future more flexible classes are likely to inherit from this base class.
#'
#' @export
Probs = R6Class("Probs",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param p_overall_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall mutation probability.
    #'   The default value is 0.3.
    #' @param p_overall_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall crossover probability.
    #'   This overall crossover probability is also used for the group structure crossover probability.
    #'   The default value is 0.7.
    #' @param p_param_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter mutation probability.
    #'   The default value is 0.2.
    #' @param p_param_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter crossover probability.
    #'   The default value is 0.5.
    #' @param p_groupstructure_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the group structure mutation probability.
    #'   The default value is 0.2.
    initialize = function(p_overall_mutate = 0.3, p_overall_crossover = 0.7, p_param_mutate = 0.2, p_param_crossover = 0.5, p_groupstructure_mutate = 0.2) {
      self$p_overall_mutate = assert_number(p_overall_mutate, lower = 0, upper = 1)
      self$p_overall_crossover = assert_number(p_overall_crossover, lower = 0, upper = 1)
      self$p_param_mutate = assert_number(p_param_mutate, lower = 0, upper = 1)
      self$p_param_crossover = assert_number(p_param_crossover, lower = 0, upper = 1)
      self$p_groupstructure_mutate = assert_number(p_groupstructure_mutate, lower = 0, upper = 1)
      self$p_groupstructure_crossover = p_overall_crossover
    },

    #' @description
    #' Updates the probabilities.
    #'
    #' Currently has no effect because probabilities are treated as non-adaptive.
    #'
    #' @return Invisible (`NULL`)
    update = function() {
      # Currently no effect

      invisible(NULL)
    },

    #' @field p_overall_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall mutation probability.
    p_overall_mutate = NULL,              # Overall probability to apply mutation to parameters and group structure (at all)

    #' @field p_overall_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall crossover probability.
    p_overall_crossover = NULL,           # Overall probability to apply crossover to parameters and group structure (at all)

    #' @field p_param_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter mutation probability.
    p_param_mutate = NULL,                # Individual probability for each parameter to mutate

    #' @field p_param_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter crossover probability.
    p_param_crossover = NULL,             # Individual probability for each parameter to undergo crossover

    #' @field p_groupstructure_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the group structure mutation probability.
    p_groupstructure_mutate = NULL,       # Individual probability for each feature in the group structure to mutate

    #' @field p_groupstructure_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the group structure crossover probability.
    p_groupstructure_crossover = NULL    # Same as p_overall_crossover because group structure crossover works globally
  )
)

