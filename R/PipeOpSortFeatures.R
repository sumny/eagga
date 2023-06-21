#' @title Sort Features alphabetically
#'
#' @description
#' A very simple [mlr3pipelines::PipeOp] to make really sure that the `$feature` vector of the `$col_roles` active binding of a [mlr3::Task] is ordered alphabetically.
#' Inherits from [mlr3pipelines::PipeOpTaskPreprocSimple].
#'
#' @export
PipeOpSortFeatures = R6Class("PipeOpSortFeatures",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (character(1))\cr
    #'   Identifier of resulting object, default `"sortfeatures"`.
    initialize = function(id = "sortfeatures") {
      super$initialize(id, param_set = ps(), param_vals = NULL, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      task$col_roles$feature = sort(task$col_roles$feature)
      task
    }
  )
)

