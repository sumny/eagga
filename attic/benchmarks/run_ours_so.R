library(data.table)
setDTthreads(1L)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(mlr3misc)
library(mlr3tuning)
library(mlr3mbo)
library(paradox)
library(bbotk)
library(eagga)

root = here::here()
source_files = file.path(root, "attic", "benchmarks", c("helpers.R", "search_spaces.R"))

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

eval_ = function(job, data, instance, ...) {
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3misc)
  library(mlr3tuning)
  library(mlr3mbo)
  library(paradox)
  library(bbotk)
  library(eagga)

  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)

  logger = lgr::get_logger("mlr3")
  logger$set_threshold("warn")
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  task = instance$task
  task_id = instance$id

  repl = job$repl
  set.seed(repl)  # same outer and inner resampling for all methods given a repl on a task
  resampling_outer = rsmp("holdout", ratio = 2/3)$instantiate(task)
  train_set = resampling_outer$train_set(1L)
  test_set = resampling_outer$test_set(1L)
  task_train = task$clone(deep = TRUE)$filter(rows = train_set)
  task_test = task$clone(deep = TRUE)$filter(rows = test_set)
  resampling_inner = rsmp("cv", folds = 5L)$instantiate(task_train)  # changed
  secs = 12L * 3600L
 
  method = job$algo.pars$method
  set.seed(job$seed) 

  results = if (method == "eagga") {
    nested_resampling_eagga(task_train, task_test = task_test, resampling_inner = resampling_inner, task_id_ = task_id, repl_ = repl, secs = secs)
  } else if (method == "eagga_md2") {
    nested_resampling_eagga_md2(task_train, task_test = task_test, resampling_inner = resampling_inner, task_id_ = task_id, repl_ = repl, secs = secs)
  } else if (method == "xgboost") {
    nested_resampling_xgboost(task_train, task_test = task_test, resampling_inner = resampling_inner, secs = secs)
  } else if (method == "ebm") {
    reticulate::use_condaenv("EBmlr3", required = TRUE)
    library(EBmlr3)
    nested_resampling_ebm(task_train, task_test = task_test, resampling_inner = resampling_inner, secs = secs)
    #nested_resampling_ebm_fallback(task_train, task_test = task_test, resampling_inner = resampling_inner, secs = secs)
  } else if (method == "glmnet") {
    nested_resampling_glmnet(task_train, task_test = task_test, resampling_inner = resampling_inner, secs = secs)
  } else if (method == "rf") {
    random_forest(task_train, task_test = task_test)
  }
  results
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_eagga_ours_so", source = source_files)
#reg = makeExperimentRegistry(file.dir = NA)
saveRegistry(reg)

ids = c(37, 43, 3903, 3913, 3904, 3918, 10093, 9946, 146819, 359955, 189922, 359962, 190392, 167120, 190137, 190410, 168350, 359975, 359972, 146820)
tasks = map(ids, function(id) {
  task = tsk("oml", task_id = id)
  if (id == 3904) {
    tmp = task$data()
    tmp = na.omit(tmp)
    task = TaskClassif$new(id = task$id, backend = tmp, target = task$target_names)
  }
  task
})
#checks = map_lgl(tasks, function(task) {
#  all(c("factor", "ordered", "logical", "POSIXct", "character") %nin% unique(task$feature_types)) && sum(task$missings()) == 0L && sum(apply(task$data(cols = task$feature_names), 2, function(x) length(unique(x)) <= 2)) == 0L
#})

instances = data.table(id = ids, task = tasks)
instances[, id_plan := 1:.N]

# add problems
prob_designs = imap(split(instances, instances$id_plan), function(instance, name) {
  addProblem(as.character(instance$id_plan), fun = function(...) list(...), seed = instance$id)
  set_names(list(instance), as.character(instance$id_plan))
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval_ algorithm (never use `eval` as a function name or have a function named `eval` in .GlobalEnv)
addAlgorithm("eval_", fun = eval_)

for (method in c("eagga", "eagga_md2", "xgboost", "ebm", "glmnet", "rf")) {
  ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = data.table(method = method)),
      repls = 10L
  )
  addJobTags(ids, method)
}

# standard resources used to submit jobs to cluster
resources.serial.default = list(max.concurrent.jobs = 9999L, ncpus = 1L)

jobs = getJobTable()
jobs[, memory := 1024L * 48L]
jobs[(problem == 11 | problem == 13 | problem == 14 | problem == 16), memory := 1024L * 64L]
jobs[, walltime := 24L * 3600L]
jobs[tags == "rf", memory := 1024L * 16L]
jobs[tags == "rf", walltime := 3600L]

submitJobs(jobs, resources = resources.serial.default)

expired = jobs[job.id %in% findExpired()$job.id]

submitJobs(expired, resources = resources.serial.default)  # for these 40 ebm the initial BO design took more than 12h so we fallback; 11, 13, 16, 19

#######################################################################################################################################################################################################

tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x
  data[, tuning_data := NULL]
  data[, task_id := job$prob.pars$id]
  data[, method := job$algo.pars$method]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/eagga_ours_so.rds")

