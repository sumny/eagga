#' @title Probs
#'
#' @description
#' Class to represent probabilities used in mutation and crossover operations.
#'
#' @details
#' This class provides a convenient way to manage and update probabilities used in mutation and crossover operations.
#' It allows you to set individual probabilities for various operations, such as overall mutation, overall crossover,
#' parameter mutation, parameter crossover, and group structure mutation.
#' Future more flexible classes will likely inherit from this base class.
#'
#' @export
Probs = R6Class("Probs",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param p_overall_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall mutation probability.
    #'   Default is 0.3.
    #' @param p_overall_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall crossover probability.
    #'   This overall crossover probability is also used for the groupstructure crossover probability.
    #'   Default is 0.7
    #' @param p_param_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter mutation probability.
    #'   Default is 0.2.
    #' @param p_param_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter crossover probability.
    #'   Default is 0.5.
    #' @param p_groupstructure_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the groupstructure mutation probability.
    #'   Default is 0.2.
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
    #' @return Invisible `NULL`
    update = function() {
      # currently no effect

      invisible(NULL)
    },

    #' @field p_overall_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall mutation probability.
    p_overall_mutate = NULL,              # overall prob to apply mutation to param and groupstructure (at all)

    #' @field p_overall_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the overall crossover probability.
    p_overall_crossover = NULL,           # overall prob to apply crossover to param and groupstructure (at all)

    #' @field p_param_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter mutation probability.
    p_param_mutate = NULL,                # individual prob for each param to mutate

    #' @field p_param_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the hyperparameter crossover probability.
    p_param_crossover = NULL,             # individual prob for each param to crossover

    #' @field p_groupstructure_mutate (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the groupstructure mutation probability.
    p_groupstructure_mutate = NULL,       # individual prob for each feature in groupstructure to mutate

    #' @field p_groupstructure_crossover (numeric(1))\cr
    #'   Numeric value between 0 and 1 specifying the groupstructure crossover probability.
    p_groupstructure_crossover = NULL    # same as p_overall_crossover because groupstructure crossover works global
  )
)

