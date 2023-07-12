# helper function to parse an xgboost tree model
# the following code is taken from the xgboost R package version 2.0.0.1 licensed under Apache License, Version 2.0
# https://github.com/dmlc/xgboost/blob/master/R-package/R/xgb.model.dt.tree.R
#
# Copyright 2023 Tianqi Chen and XBGoost Contributors
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# The following minimal changes were made:
# `text = xgb.dump(model = model, with_stats = TRUE)`
# changed to
# `text = xgboost::xgb.dump(model = model, with_stats = TRUE)
# `sum(grepl('leaf=(\\d+)|leaf=(\\d+)', text)) < 1)`
# changed to
# `sum(grepl('leaf=([+-]\\d+)|leaf=(\\d+)', text)) < 1)
# Changed assign operator `<-` to `=`
xgb_model_dt_tree = function(feature_names = NULL, model = NULL, text = NULL, trees = NULL, use_int_id = FALSE, ...) {
  if (!inherits(model, "xgb.Booster") && !is.character(text)) {
    stop("Either 'model' must be an object of class xgb.Booster\n",
         "  or 'text' must be a character vector with the result of xgb.dump\n",
         "  (or NULL if 'model' was provided).")
  }

  if (is.null(feature_names) && !is.null(model) && !is.null(model$feature_names))
    feature_names = model$feature_names

  if (!(is.null(feature_names) || is.character(feature_names))) {
    stop("feature_names: must be a character vector")
  }

  if (!(is.null(trees) || is.numeric(trees))) {
    stop("trees: must be a vector of integers.")
  }

  if (is.null(text)){
    text = xgboost::xgb.dump(model = model, with_stats = TRUE)
  }

  if (length(text) < 2 ||
      sum(grepl('leaf=([+-]\\d+)|leaf=(\\d+)', text)) < 1) {
    stop("Non-tree model detected! This function can only be used with tree models.")
  }

  position = which(grepl("booster", text, fixed = TRUE))

  add.tree.id = function(node, tree) if (use_int_id) node else paste(tree, node, sep = "-")

  anynumber_regex = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"

  td = data.table(t = text)
  td[position, Tree := 1L]
  td[, Tree := cumsum(ifelse(is.na(Tree), 0L, Tree)) - 1L]

  if (is.null(trees)) {
    trees = 0:max(td$Tree)
  } else {
    trees = trees[trees >= 0 & trees <= max(td$Tree)]
  }
  td = td[Tree %in% trees & !grepl('^booster', t)]

  td[, Node := as.integer(sub("^([0-9]+):.*", "\\1", t))]
  if (!use_int_id) td[, ID := add.tree.id(Node, Tree)]
  td[, isLeaf := grepl("leaf", t, fixed = TRUE)]

  # parse branch lines
  branch_rx = paste0("f(\\d+)<(", anynumber_regex, ")\\] yes=(\\d+),no=(\\d+),missing=(\\d+),",
                      "gain=(", anynumber_regex, "),cover=(", anynumber_regex, ")")
  branch_cols = c("Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover")
  td[
    isLeaf == FALSE,
    (branch_cols) := {
      matches = regmatches(t, regexec(branch_rx, t))
      # skip some indices with spurious capture groups from anynumber_regex
      xtr = do.call(rbind, matches)[, c(2, 3, 5, 6, 7, 8, 10), drop = FALSE]
      xtr[, 3:5] = add.tree.id(xtr[, 3:5], Tree)
      if (length(xtr) == 0) {
        as.data.table(
          list(Feature = "NA", Split = "NA", Yes = "NA", No = "NA", Missing = "NA", Quality = "NA", Cover = "NA")
        )
      } else {
        as.data.table(xtr)
      }
    }
  ]

  # assign feature_names when available
  is_stump = function() {
    return(length(td$Feature) == 1 && is.na(td$Feature))
  }
  if (!is.null(feature_names) && !is_stump()) {
    if (length(feature_names) <= max(as.numeric(td$Feature), na.rm = TRUE))
      stop("feature_names has less elements than there are features used in the model")
    td[isLeaf == FALSE, Feature := feature_names[as.numeric(Feature) + 1]]
  }

  # parse leaf lines
  leaf_rx = paste0("leaf=(", anynumber_regex, "),cover=(", anynumber_regex, ")")
  leaf_cols = c("Feature", "Quality", "Cover")
  td[
    isLeaf == TRUE,
    (leaf_cols) := {
      matches = regmatches(t, regexec(leaf_rx, t))
      xtr = do.call(rbind, matches)[, c(2, 4)]
      if (length(xtr) == 2) {
        c("Leaf", as.data.table(xtr[1]), as.data.table(xtr[2]))
      } else {
        c("Leaf", as.data.table(xtr))
      }
    }
  ]

  # convert some columns to numeric
  numeric_cols = c("Split", "Quality", "Cover")
  td[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
  if (use_int_id) {
    int_cols = c("Yes", "No", "Missing")
    td[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  }

  td[, t := NULL]
  td[, isLeaf := NULL]

  td[order(Tree, Node)]
}

# helper function to get interactions of features in an xgboost tree model
# NOTE: currently we only use it so see which pairs of features interact
#       we could speed things up drastically by not doing the gain calculations
get_interactions = function(xgb_model, option = "pairs") {
  if (option == "pairs") {
    gain_table = calculate_pairs_gain_table(xgb_model)
  }
  gain_table
}

# helper function to extract pairs of features used in splits and their gain in an xgboost tree model
calculate_pairs_gain_table = function(xgb_model) {
  list_of_trees = calculate_gain(xgb_model)
  trees = rbindlist(list_of_trees, fill = TRUE)
  if ("name_pair" %nin% colnames(trees)) {
    trees[, name_pair := NA_character_]
    importance = data.table(Parent = character(), Child = character(), Sum_gain = numeric(), Frequency = integer())
    return(importance)  # early exit
  }

  importance_count = data.table(table(trees[, "name_pair"], dnn = "name_pair"))
  importance_gain = trees[, .(Sum_gain = sum(childs_gain)), by = "name_pair"]
  importance = merge(importance_count, y = importance_gain, by = "name_pair")
  importance = importance[, `:=`(Parent = map_chr(strsplit(importance[, name_pair], "[:]"), 1L), Child = map_chr(strsplit(importance[, name_pair], "[:]"), 2L))]
  importance = importance[, -1L]
  setorderv(importance, cols = "Sum_gain", order = -1L)

  importance[, .(Parent, Child, Sum_gain, Frequency = N)]
}

# helper function to calculate the gain of each split in each tree of an xgboost tree model
calculate_gain = function(xgbmodel) {
  trees = get_table_of_trees(xgbmodel)
  trees[, leaf := Feature == "Leaf"]
  trees$depth = 0L
  list_of_trees = split(trees, as.factor(trees$Tree))

  for (tree in list_of_trees) {
    num_nodes = nrow(tree)
    non_leaf_rows = which(tree[, leaf] == FALSE)
    for (r in non_leaf_rows) {
      left = tree[r, Yes]
      right = tree[r, No]
      if (tree[ID == left, leaf] == FALSE) {
        tree[ID == left,`:=`(parents_gain = tree[r, Quality], parents_cover = tree[r, Cover], name_pair = paste(tree[r, Feature], tree[ID == left, Feature], sep = ":"), childs_gain = Quality, depth = tree[r, depth] + 1L, parents_name = tree[r, Feature])]
        tree[ID == left, interaction := ((parents_gain < childs_gain) & (Feature != parents_name))]
      }
      if (tree[ID == right, leaf] == FALSE) {
        tree[ID == right, `:=`(parents_gain = tree[r, Quality], parents_cover = tree[r, Cover], name_pair = paste(tree[r, Feature], tree[ID == right, Feature], sep = ":"), childs_gain = Quality, depth = tree[r, depth] + 1L, parents_name = tree[r, Feature])]
        tree[ID == right, interaction := ((parents_gain < childs_gain) & (Feature != parents_name))]
      }
    }
  }
  list_of_trees
}

# helper function to represent the internal structure of an xgboost tree model
get_table_of_trees = function(model, ...) {
  if (class(model)[1L] == "xgb.Booster") {
    xgb_model_dt_tree(model = model, ...)[]
  }
  # could add support for other tree based models
}

