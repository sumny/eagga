#' Interaction Detector
#'
#' Detects important pairwise interactions using a simple interaction detector inspired by the more sophisticated
#' methodology proposed in Lou et al. (2013).
#'
#' In contrast to the original FAST algorithm, this detector is a crude implementation.
#' It discretizes all features based on relatively small grid sizes, and the same unique grid values are used as
#' potential cut points to find the best pair of cut points for each feature pair that results in the largest decrease
#' in the residual sum of squares.
#'
#' Additionally, please note that targets are simply converted to numeric values in the case of a [mlr3::TaskClassif], instead of working on
#' logits or model outputs of a previously computed proxy model.
#'
#' Overall, it is important to remember that this detector is solely used to initialize the group structures of the initial population
#' within [TunerEAGGA]. Therefore, some impreciseness is acceptable.
#'
#' This interaction detector only works with integer or numeric features. Logical features must be converted to integers.
#'
#' @references
#' * `r format_bib("lou_2013")`
#'
#' @export
InteractionDetector = R6Class("InteractionDetector",
  # NOTE: could / should also be replicated and rely on subsampling to prevent some potential overfitting
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task.
    #' @param grid_size (integer(1))\cr
    #'   The grid size used to construct a grid for each feature.
    #'   The default value is `11`.
    #'   A grid for a feature is constructed by computing quantiles on the feature with probabilities ranging from
    #'   `seq(from = 0, to = 1, length.out = grid_size)`.
    #'   If there are not at least three unique grid values that can be found based on quantiles (resulting in at least two bins
    #'   for the discretized feature), the range of the feature is split into two intervals of equal width, and the corresponding
    #'   interval points are used as grid values as a fallback.
    #'   For integer features, the resulting quantiles are rounded to the nearest integer.
    #'   Grid values are always assumed to be unique.
    #'   Therefore, the actual number of unique grid values may be smaller than `grid_size`.
    initialize = function(task, grid_size = 11L) {
      assert_task(task, feature_types = c("integer", "numeric"))
      assert_int(grid_size, lower = 1L)
      xs = task$data(cols = task$feature_names)
      feature_names = task$feature_names
      feature_types = task$feature_types
      y = as.numeric(task$data(cols = task$target_names)[[1L]])  # regardless of regr or classif
      self$xs = xs
      self$n_features = length(feature_names)
      self$feature_names = feature_names
      self$feature_types = feature_types
      self$y = y

      self$grids = setNames(map(self$feature_names, function(feature_name) {
        feature_type = feature_types[id == feature_name][["type"]]
        if (feature_type == "numeric") {
          grid_values = unique(unname(quantile(self$xs[[feature_name]], probs = seq(from = 0, to = 1, length.out = grid_size))))
          if (length(grid_values) <= 2L) {
            grid_values = seq(from = min(self$xs[[feature_name]]), to = max(self$xs[[feature_name]]), length.out = 3L)
          }
        } else if (feature_type == "integer") {
          grid_values = unique(round(unname(quantile(self$xs[[feature_name]], probs = seq(from = 0, to = 1, length.out = grid_size)))))
          if (length(grid_values) <= 2L) {
            grid_values = unique(round(seq(from = min(self$xs[[feature_name]]), to = max(self$xs[[feature_name]]), length.out = 3L)))
          }
        }
        grid_values
      }), nm = self$feature_names)

      cuts = vector("list", length = self$n_features)
      for (i in seq_len(self$n_features)) {
        feature_name = self$feature_names[[i]]
        discretization = cut(self$xs[[feature_name]], breaks = self$grids[[feature_name]], include.lowest = TRUE)
        set(self$xs, j = feature_name, value = as.integer(discretization))
        self$cuts[[i]] = seq_along(levels(discretization))
      }
    },

    #' @field xs ([data.table::data.table])\cr
    #'   Data.table of features from the [mlr3::Task].
    #'   Note that feature values of each feature are discretized based on the corresponding feature grid in `$grids`.
    xs = NULL,
    
    #' @field n_features (integer(1))\cr
    #'   Number of features in the [mlr3::Task].
    n_features = NULL,
    
    #' @field feature_names (character()) \cr
    #'   Names of the features in the [mlr3::Task].
    feature_names = NULL,
    
    #' @field feature_types (character()) \cr
    #'   Original types of the features in the [mlr3::Task] prior to discretization.
    feature_types = NULL,
    
    #' @field y (numeric())\cr
    #'   Numeric target vector of the [mlr3::Task].
    #'   In the case of a [mlr3::TaskClassif], the label is simply converted to numeric.
    y = NULL,
    
    #' @field grids (list())\cr
    #'   List of grids containing a grid for each feature.
    #'   Grids are constructed by computing quantiles on the feature with probabilities ranging from
    #'   `seq(from = 0, to = 1, length.out = grid_size)`.
    #'   For integer features, the resulting quantiles are rounded to the nearest integer, and the unique
    #'   values are taken, which may result in a grid of smaller size than `grid_size`.
    grids = NULL,
    
    #' @field cuts (list())\cr
    #'   List of cut points containing cut points for each feature.
    #'   Cut points for features are based on their unique values after discretization using their corresponding grid.
    cuts = NULL,
    
    #' @field rss (matrix())\cr
    #'   Symmetric numeric matrix of dimensions `n_features` by `n_features` containing the reduction in the residual sum
    #'   of squares for each pair of features.
    #'   `NULL` after construction.
    #'   To compute this, use `$compute_best_rss`.
    rss = NULL,
    
    #' @description
    #' Method to compute the reduction in the residual sum of squares for each pair of features, overwriting `$rss`.
    #'
    #' @return Invisible (NULL)
    compute_best_rss = function() {
      pb = progress_bar$new(format = "Detecting interactions [:bar] :percent eta: :eta", total = (self$n_features * self$n_features - 1L) / 2L)
      rss = matrix(0, nrow = self$n_features, ncol = self$n_features)
        for (i in seq_len(self$n_features)) {
          for (j in seq_len(self$n_features)) {
            if (i < j) {
              pb$tick()
              rss[i, j] = compute_best_rss_pairwise(self$xs[[i]], self$xs[[j]], self$y, self$cuts[[i]], self$cuts[[j]])
            }
          }
        }
      self$rss = rss
      invisible(NULL)
    },

    #' @description
    #' Retrieves equivalence classes (groups of features) based on the top k most important pairwise interactions.
    #' The interactions are determined by the reduction in the residual sum of squares for each pair of features.
    #'
    #' @param k (integer(1))\cr
    #'   The number of top interactions to consider.
    #'   The default value is `1`.
    #' @param features (character() | NULL)\cr
    #'   The features to consider for detecting interactions.
    #'   If not provided, all features will be used (default).
    #'
    #' @return
    #' A named integer vector indicating the equivalence class each feature belongs to.
    #'
    get_eqcs_from_top_k = function(k = 1L, features = NULL) {
      assert_subset(features, choice = self$feature_names)
      if (is.null(features)) features = self$feature_names
      n_features = length(features)
      assert_int(k, lower = 1L, upper = (n_features * (n_features - 1L)) / 2L)
      rss = self$rss[match(features, self$feature_names), match(features, self$feature_names)]
      top_rss = sort(rss[upper.tri(rss)])[seq_len(k)]
      top_interactions = unlist(map(top_rss, function(x) asplit(which(rss == x, arr.ind = TRUE), MARGIN = 1L)), recursive = FALSE)
      relation_matrix = matrix(0, nrow = n_features, ncol = n_features)
      colnames(relation_matrix) = rownames(relation_matrix) = features
      for (interaction in top_interactions) {
        relation_matrix[interaction[1L], interaction[2L]] = 1
        relation_matrix[interaction[2L], interaction[1L]] = 1  # make symmetric
      }
      diag(relation_matrix) = 1  # make reflexive
      relation = relation(list(features, features), incidence = relation_matrix)
      relation = transitive_closure(relation)  # make transitive
      belonging = get_class_ids_from_incidence(relation_incidence(relation)[features, features, drop = FALSE])
      belonging
    }
  )
)

# CH_i^t(v) if lower.tail else \bar{CH}_i^t 
compute_cumsum_targets = function(x, v, y, lower.tail = TRUE) {
  if (lower.tail) {
    sum(y[x <= v])
  } else {
    sum(y[x > v])
  }
}

# CH_i^w(v) if lower.tail else \bar{CH}_i^w 
compute_cumsum_counts = function(x, v, lower.tail = TRUE) {
  if (lower.tail) {
    sum(x <= v)
  } else {
    sum(x > v)
  }
}

# H_ij^t(u, v)
compute_bivar_sum_targets = function(xi, xj, y, v, u) {
  sum(y[xi == v & xj == u])
}

# H_ij^t(u, v)
compute_bivar_sum_counts = function(xi, xj, v, u) {
  sum(xi == v & xj == u)
}

# CH_ij^t(u, v)
compute_bivar_cumsum_targets = function(xi, xj, y, v, u) {
  sum(y[xi <= v & xj <= u])
}

# CH_ij^t(u, v)
compute_bivar_cumsum_counts = function(xi, xj, v, u) {
  sum(xi <= v & xj <= u)
}

compute_values_targets = function(xi, xj, y, v, u, a) {
  cumsum_targets_i = compute_cumsum_targets(xi, v, y)
  cumsum_targets_j = compute_cumsum_targets(xj, u, y)
  cumsum_targets_i_upper = compute_cumsum_targets(xi, v, y, lower.tail = FALSE)
  b = cumsum_targets_i - a
  c = cumsum_targets_j - a
  d = cumsum_targets_i_upper - c
  c(a, b, c, d)
}

compute_values_counts = function(xi, xj, v, u, a) {
  cumsum_counts_i = compute_cumsum_counts(xi, v)
  cumsum_counts_j = compute_cumsum_counts(xj, u)
  cumsum_counts_i_upper = compute_cumsum_counts(xi, v, lower.tail = FALSE)
  b = cumsum_counts_i - a
  c = cumsum_counts_j - a
  d = cumsum_counts_i_upper - c
  c(a, b, c, d)
}

na.replace = function(x, value = 0) {
  x[is.na(x) | !is.finite(x)] = value
  x
}

compute_best_rss_pairwise = function(xi, xj, y, cuts_i, cuts_j) {
  stopifnot(all(xi %in% cuts_i))
  stopifnot(all(xj %in% cuts_j))

  n_cuts_i = length(cuts_i)
  n_cuts_j = length(cuts_j)

  # construct lookup table targets
  sum_t = 0
  a_list_t = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  lookup_list_t = lapply(seq_len(n_cuts_i), function(x) vector("list", length = n_cuts_j))
  
  for (q in 1:n_cuts_j) {
    sum_t = sum_t + compute_bivar_sum_targets(xi, xj, y, cuts_i[1L], cuts_j[q])
    a_list_t[[1L]][q] = sum_t
    lookup_list_t[[1L]][[q]] = compute_values_targets(xi, xj, y, cuts_i[1L], cuts_j[q], a_list_t[[1L]][q])
  }
  
  for (p in 2:n_cuts_i) {
    sum_t = 0
    for (q in 1:n_cuts_j) {
      sum_t = sum_t + compute_bivar_sum_targets(xi, xj, y, cuts_i[p], cuts_j[q])
      a_list_t[[p]][q] = sum_t + a_list_t[[p - 1L]][q]
      lookup_list_t[[p]][[q]] = compute_values_targets(xi, xj, y, cuts_i[p], cuts_j[q], a_list_t[[p]][q])
    }
  }
  
  # construct lookup table counts
  sum_w = 0
  a_list_w = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  lookup_list_w = lapply(seq_len(n_cuts_i), function(x) vector("list", length = n_cuts_j))
  
  for (q in 1:n_cuts_j) {
    sum_w = sum_w + compute_bivar_sum_counts(xi, xj, cuts_i[1L], cuts_j[q])
    a_list_w[[1L]][q] = sum_w
    lookup_list_w[[1L]][[q]] = compute_values_counts(xi, xj, cuts_i[1L], cuts_j[q], a_list_w[[1L]][q])
  }
  
  for (p in 2:n_cuts_i) {
    sum_w = 0
    for (q in 1:n_cuts_j) {
      sum_w = sum_w + compute_bivar_sum_counts(xi, xj, cuts_i[p], cuts_j[q])
      a_list_w[[p]][q] = sum_w + a_list_w[[p - 1L]][q]
      lookup_list_w[[p]][[q]] = compute_values_counts(xi, xj, cuts_i[p], cuts_j[q], a_list_w[[p]][q])
    }
  }
  
  rss = lapply(seq_len(n_cuts_i), function(x) rep(NA_real_, n_cuts_j))
  for (i in seq_len(n_cuts_i)) {
    for (j in seq_len(n_cuts_j)) {
      tijs = na.replace(lookup_list_t[[i]][[j]] / lookup_list_w[[i]][[j]])  # weights can be zero and we divide by them
      rss[[i]][j] = sum(tijs^2 * lookup_list_w[[i]][[j]]) - 2 * sum(tijs * lookup_list_t[[i]][[j]])
    }
  }
  
  best_rss = min(unlist(rss))  # we computed the "reduction" in RSS therefore min is best
  best_rss
}

