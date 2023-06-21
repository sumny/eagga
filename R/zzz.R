#' @importFrom R6 R6Class
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import bbotk
#' @import lgr
#' @import mlr3
#' @import mlr3learners
#' @import mlr3pipelines
#' @import mlr3tuning
#' @import relations
#' @import mlr3filters
#' @import xgboost
#' @import progress
#' @importFrom stats setNames runif dnorm pnorm rnorm rgeom var
#' @importFrom utils stack
#' @importFrom R.utils withTimeout

register_mlr3tuning = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  x$add("eagga", TunerEAGGA)
} # nocov end

register_mlr3pipelines = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("sortfeatures", PipeOpSortFeatures)
} # nocov end

register_mlr3 = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("selected_features_proxy", function() MeasureSelectedFeaturesProxy$new())
  x$add("selected_interactions_proxy", function() MeasureSelectedInteractionsProxy$new())
  x$add("selected_non_monotone_proxy", function() MeasureSelectedNonMonotoneProxy$new())
}

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  register_namespace_callback(pkgname, "mlr3tuning", register_mlr3tuning)
  register_namespace_callback(pkgname, "mlr3pipelines", register_mlr3pipelines)
  register_namespace_callback(pkgname, "mlr3", register_mlr3)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

.onUnload = function(libpaths) { # nolint
  # nocov start
   mlr3tuning::mlr_tuners$remove("eagga")
   mlr3pipelines::mlr_pipeops$remove("sortfeatures")
   mlr3::mlr_measures$remove("selected_features_proxy")
   mlr3::mlr_measures$remove("selected_interactions_proxy")
   mlr3::mlr_measures$remove("selected_non_monotone_proxy")
} # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("id", "Parent", "Child", "var", "stack", "Child", "Tree", "Feature", "Node", "Quality", "Yes", "No", "Missing", "ID", "isLeaf", "feature"))

#if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
#  leanify_package()
#}

