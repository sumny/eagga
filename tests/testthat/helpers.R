lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Simple binary classif task
create_binary_classif_task = function(n, seed = 0) {
  set.seed(seed)
  x1 = rnorm(n)
  x2 = rnorm(n, sd = 1)
  x3 = rnorm(n, mean = 10)
  x4 = runif(n)
  x5 = runif(n, min = 0, max = 10)
  y = x1 + x2 + x1 * x2 + x3 * (x4 - 0.5) + rnorm(n)
  label = as.factor(ifelse(plogis(y) <= 0.5, yes = "0", no = "1"))
  task = TaskClassif$new("test_classif", backend = data.table(label = label, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5), target = "label")
  task
}

# Simple regr task
create_regr_task = function(n, seed = 0) {
  set.seed(seed)
  x1 = rnorm(n)
  x2 = rnorm(n, sd = 1)
  x3 = rnorm(n, mean = 10)
  x4 = runif(n)
  x5 = runif(n, min = 0, max = 10)
  y = x1 + x2 + x1 * x2 + x3 * (x4 - 0.5) + rnorm(n)
  task = TaskRegr$new("test_classif", backend = data.table(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5), target = "y")
  task
}

