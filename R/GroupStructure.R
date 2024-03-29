#' @title Group Structure
#'
#' @description
#' Class to represent a group structure.
#'
#' @export
GroupStructure = R6Class("GroupStructure",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task)\cr
    #'   Task.
    #' @param n_selected (integer(1))\cr
    #'   Number of selected features.
    #' @param scores ([data.table::data.table])\cr
    #'   data.table with as many rows as features and two columns `feature` and `score` with `feature` being the id of the feature and `score` being the feature importance score.
    #' @param interaction_detector (InteractionDetector)\cr
    #'   Interaction detector. 
    #' @param unconstrained_weight_table ([data.table::data.table])\cr
    #'   data.table with as many rows as features and two columns `feature_name` and `unconstrained_weight` with `feature_name` being the id of the feature and `unconstrained_weight` being the score based on an [MonotonicityDetector].
    #' @param unconstrained (logical(1)) \cr
    #'   Whether an unconstrained group structure should be created.
    #'   This means that all features are selected and all features are allowed to interact and no features are
    #'   constrained with respect to a monotone feature effect.
    initialize = function(task, n_selected = NULL, scores = NULL, interaction_detector = NULL, unconstrained_weight_table = NULL, unconstrained = FALSE) {
      self$allow_all_unselected = FALSE  # default at least two groups (unselected and one selected)
      # checks and feature names
      assert_task(task, feature_types = c("integer", "numeric"))
      assert_int(n_selected, lower = 1L, upper = nrow(task$feature_types), null.ok = TRUE)  # null.ok?
      assert_data_table(scores, null.ok = TRUE)
      assert_r6(interaction_detector, classes = "InteractionDetector", null.ok = TRUE)  # null.ok?
      assert_data_table(unconstrained_weight_table, null.ok = TRUE)  # null.ok?
      assert_flag(unconstrained)
      feature_names = sort(task$feature_names)  # NOTE: we assume that features in the task will be sorted alphabetically prior to training, e.g., via PipeOpSortFeatures
      n_features = length(feature_names)
      self$feature_names = feature_names

      if (unconstrained) {
        selected_features = feature_names
        eqcs = list(features = selected_features, eqcs = 1L, belonging = rep(1L, length(selected_features)))
        # create group structure (including the first group being unselected)
        self$groups = self$get_full_group_structure(eqcs, unselected_features = setdiff(feature_names, selected_features))
        self$monotone_eqcs = data.table(eqcs = self$groups$eqcs, monotonicity = c(NA_integer_, 0L))
      } else {

        # sample selected features based on scores
        selected_features = sort(sample(scores$feature, size = n_selected, replace = FALSE, prob = scores$score))

        # eqcs
        if (n_selected == 1L) {
          eqcs = list(features = selected_features, eqcs = 1L, belonging = 1L)
        } else {
          task_selected = task$clone(deep = TRUE)
          task_selected$col_roles$feature = selected_features
          n_interactions_prob = 1 / get_n_interactions_rpart(task_selected)
          k = sample_from_truncated_geom(n_interactions_prob, lower = 1L, upper = (n_selected * (n_selected - 1L)) / 2L)  # number of pairwise interactions sampled from truncated geometric distribution

          belonging = interaction_detector$get_eqcs_from_top_k(k = k, features = selected_features)
          belonging = match(belonging, sort(unique(belonging)))  # make eqcs start at 1

          eqcs = unique(belonging)
          eqcs = list(features = selected_features, eqcs = sort(eqcs), belonging = belonging)
        }

        # create group structure (including the first group being unselected)
        self$groups = self$get_full_group_structure(eqcs, unselected_features = setdiff(feature_names, selected_features))

        # randomly sample monotonicity constraints of partitions using weights from unconstrained_weight_table
        # base probabilities are 0.2 for constrained, 0.8 for unconstrained
        group_weights = map_dbl(get_eqcs(self$groups)[-1L], function(group) {  # excluding the unselected group
          weights = unconstrained_weight_table[match(group, feature_name), unconstrained_weight]
          mean(weights)
        })
        stopifnot(length(group_weights) == (length(self$groups$eqcs) - 1L))
        monotone_eqcs = map_int(group_weights, function(group_weight) {
          sample(c(0L, 1L), size = 1L, prob = c(group_weight, 1 - group_weight))
        })
        monotone_eqcs = c(NA_integer_, monotone_eqcs)  # first one is the unselected group
        self$monotone_eqcs = data.table(eqcs = self$groups$eqcs, monotonicity = monotone_eqcs)
      }
    },

    #' @field feature_names (character()) \cr
    #'   The names of the features of the [mlr3::Task].
    feature_names = NULL,

    #' @description
    #' Method to get a more human readable representation of the group structure.
    #'
    #' @return A (list()) of character vectors with each character vector containing names of features that are in the same group.
    get_groups = function() {
      map(self$groups$eqcs, function(eqc) {
        self$groups$features[self$groups$belonging == eqc]
      })
    },

    #' @description
    #' Method to get the pairwise interaction matrix of features.
    #'
    #' @return A (matrix()) filled with integers indicating allowance of pairwise interaction of selected features.
    get_matrix = function() {
      x = self$get_groups()[-1L]  # first one is the unselected

      features = unique(unlist(x))
      stopifnot(setequal(features, self$selected_features))
      n_features = length(features)
      I = diag(1, nrow = n_features, ncol = n_features)
      rownames(I) = colnames(I) = features
      for (i in seq_along(x)) {
        indices = match(x[[i]], features)
        # if eqc only has one member do nothing
        if (length(indices) >= 2L) {
          interactions = utils::combn(indices, m = 2L)
          I[t(interactions)] = 1
        }
      }
      I[lower.tri(I)] = t(I)[lower.tri(I)]
      reorder = match(rownames(I), self$selected_features)
      I[reorder, reorder]
    },

    #' @description
    #' Method to create a [mlr3pipelines::Selector] of selected features.
    #'
    #' @return ([mlr3pipelines::Selector])
    create_selector = function() {
      s = selector_name(self$selected_features)
      attr(s, "n_selected") = self$n_selected
      attr(s, "n_selected_total") = self$n_features
      s
    },

    #' @description
    #' Method to create a list of interaction constraints of features.
    #' 
    #' @return (list()) of integer vectors of feature indices allowed to interact with each other.
    create_interaction_constraints = function() {
      I = list(I = self$get_matrix(), classes = map(self$get_groups()[-1L], function(x) match(x, self$selected_features)))
      interaction_constraints = I$classes
      n_interactions = sum(I$I[upper.tri(I$I)])
      stopifnot(nrow(I$I) == self$n_selected)  # I is only dim n_selected x n_selected
      n_interactions_total = self$n_features * (self$n_features - 1L) / 2L  # number of elements of upper tri without diag, but for all features
      interaction_constraints = map(interaction_constraints, function(x) x - 1L)  # must start at 0
      attr(interaction_constraints, "n_interactions") = n_interactions
      attr(interaction_constraints, "n_interactions_total") = n_interactions_total
      interaction_constraints
    },

    #' @description
    #' Method to create an integer vector of monotonicity constraints of selected features.
    #'
    #' @return Named (integer()) vector of monotonicity constraints of selected features.
    create_monotonicity_constraints = function() {
      monotone_features = na.omit(self$monotone_features)
      stopifnot(setequal(monotone_features$feature, self$selected_features))
      monotonicity_constraints = setNames(monotone_features[["monotonicity"]], nm = monotone_features[["feature"]])[self$selected_features]
      n_non_monotone = sum(monotonicity_constraints == 0)
      n_non_monotone_total = self$n_features
      attr(monotonicity_constraints, "n_non_monotone") = n_non_monotone
      attr(monotonicity_constraints, "n_non_monotone_total") = n_non_monotone_total
      monotonicity_constraints
    },

    #' @description
    #' Method to create the full group structure based on the equivalence classes of features allowed to interact and the unselected features forming the first group.
    #'
    #' @param eqcs (list())\cr
    #'   List containing a (character()) vector of selected features, an (integer()) vector of equivalence classes ids and an (integer()) vector indicating the equivalence class each feature belongs to.
    #' @param unselected_features (character())\cr
    #'   Character vector describing the set on unselected features
    #'
    #' @return (list()) similarly as `eqcs` but updated so that the unselected features now resample the first group (equivalence class)
    get_full_group_structure = function(eqcs, unselected_features) {
      eqcs$eqcs = eqcs$eqcs + 1L  # 1 is now the unselected group
      eqcs$belonging = eqcs$belonging + 1L

      # add unselected as first group
      eqcs$features = c(unselected_features, eqcs$features)
      eqcs$eqcs = c(1L, eqcs$eqcs)
      eqcs$belonging = c(rep(1L, length(unselected_features)), eqcs$belonging)

      # reorder features and belonging alphabetically
      reorder = match(self$feature_names, eqcs$features)
      eqcs$features = eqcs$features[reorder]
      eqcs$belonging = eqcs$belonging[reorder]
      eqcs
    },

   #' @description
   #' Performs mutations on the group structure by creating a new group,
   #' destroying a group, shuffling membership, or mutating group membership.
   #'
   #' @param p (numeric(1))\cr
   #'   Numeric value between 0 and 1 specifying the mutation probability.
   #'
   #' @return Invisible (NULL)
    mutate = function(p) {
      # create a new group, taking members from 1 (monotone_eqcs then 0)
      # destroy a group, put members in 1
      # shuffle membership
      # mutate group membership

      old_groups = self$groups
      old_monotone_eqcs = self$monotone_eqcs

      tryCatch(
      {
        groups = self$groups
        group_lookup = data.table(groups = groups$eqcs, reordered = seq_along(groups$eqcs))
        change_belonging = runif(length(groups$features)) < p
        if (sum(change_belonging) > 0) {
          new_group_id = max(groups$eqcs) + 1L
          group_lookup = rbind(group_lookup, data.table(groups = new_group_id, reordered = new_group_id))
          monotone_eqcs = rbind(self$monotone_eqcs, data.table(eqcs = new_group_id, monotonicity = 0L))
          new_belonging = sample(c(groups$eqcs, new_group_id), size = sum(change_belonging), replace = TRUE)
          belonging = groups$belonging
          belonging[change_belonging] = new_belonging

          # some older groups may now be unselected
          group_lookup = group_lookup[groups %in% unique(c(1L, belonging)), ]  # it may happen that "1_1" is empty
          group_lookup[, reordered := seq_len(.N)]

          belonging = group_lookup$reordered[match(belonging, group_lookup$groups)]
          eqcs = unique(belonging)
          self$groups = list(features = groups$features, eqcs = unique(c(1L, sort(eqcs))), belonging = belonging)

          # update group attributes (monotonicity)
          orig_eqcs = group_lookup$groups[match(eqcs, group_lookup$reordered)]
          monotone_eqcs = monotone_eqcs[match(unique(c(1L, orig_eqcs)), monotone_eqcs$eqcs), ]  # it may happen that "1_1" is empty
          monotone_eqcs[, eqcs := group_lookup$reordered[match(monotone_eqcs$eqcs, group_lookup$groups)]]
          setorderv(monotone_eqcs, "eqcs")
          self$monotone_eqcs = monotone_eqcs
        }
        change_monotonicity = runif(nrow(self$monotone_eqcs) -1L) < p
        if (sum(change_monotonicity) > 0) {
          new_monotonicity = sample(c(0L, 1L), size = sum(change_monotonicity), replace = TRUE)
          # + 1 because first row is always NA
          self$monotone_eqcs = self$monotone_eqcs[which(change_monotonicity) + 1L, monotonicity := new_monotonicity]
        }
      }, error = function(error_condition) {
        warning(error_condition$message)
        warning("Resetting due to error in mutation.")
        self$groups = old_groups
        self$monotone_eqcs = old_monotone_eqcs
      })

      invisible(NULL)
    },

    #' @description
    #' Performs crossover between the groups of two [GroupStructure] objects.
    #' First parent is given by groups of the object itself (self$groups).
    #' Combines and exchanges groups between the parent objects based on the specified crossing sections.
    #'
    #' @param parent2 (list(1))\cr
    #'   A group of a [GroupStructure] object representing the second parent.
    #' @param crossing_sections (list(1))\cr
    #'   A list containing the crossing sections for the first parent (`crossing_section1`) and second parent (`crossing_section2`).
    #'   The crossing sections define the boundaries of groups to be exchanged between the parents.
    #'   Each crossing section is an integer vector of length 2, indicating the start and end points of the crossing section.
    #'
    #' @return Invisible (NULL)
    crossover = function(parent2, crossing_sections) {
      # parent1 is self$groups
      # parent2 is $groups of another GroupStructure
      # GGA, groups are {not_selected, eqc1, ..., eqcl} not_selected must be the first
      old_groups = self$groups
      old_monotone_eqcs = self$monotone_eqcs

      tryCatch(
      {
        parent1_monotone_eqcs = copy(self$monotone_eqcs)
        parent1_monotone_eqcs[, eqcs := paste0("1_", eqcs)]
        parent2_monotone_eqcs = copy(parent2$monotone_eqcs)
        parent2_monotone_eqcs[, eqcs := paste0("2_", eqcs)]

        parent1 = copy(self$groups)
        parent1$eqcs = paste0("1_", parent1$eqcs)
        parent1$belonging = paste0("1_", parent1$belonging)
        parent2 = copy(parent2$groups)
        parent2$eqcs = paste0("2_", parent2$eqcs)
        parent2$belonging = paste0("2_", parent2$belonging)

        stopifnot(parent1$features == parent2$features)

        crossing_section1 = crossing_sections[[1L]]
        crossing_section2 = crossing_sections[[2L]]
        if (length(parent1$eqcs) == 1L) {
          left_section = 1
          right_section = 1
        } else {
          left_section = parent1$eqcs[1:crossing_section1[1L]]  # AB
          right_section = parent1$eqcs[(crossing_section1[1L] + 1):length(parent1$eqcs)]  # DE
        }

        groups_to_be_injected = parent2$eqcs[crossing_section2[1L]:crossing_section2[2L]]  # bcd
        features_in_injected_groups = which(parent2$belonging %in% groups_to_be_injected)  # which features are contained in the to be injected groups of parent2
        if ("2_1" %in% groups_to_be_injected) {
          # if the unselected group should be injected we add it to the unselected group of the parent instead of
          # inserting it as a new group
          groups_to_be_injected[groups_to_be_injected == "2_1"] = "1_1"
          parent2$belonging[parent2$belonging == "2_1"] = "1_1"
        }

        # new groups would look like this
        groups = c(left_section, groups_to_be_injected, right_section)  # AB | bcd | DE
        groups = unique(groups)  # due to "2_1" %in% groups_to_be_injected handling above
        group_lookup = data.table(groups = groups, reordered = seq_along(groups))

        # belonging of features now has changed after insertion of groups
        belonging = parent1$belonging
        for (group in groups_to_be_injected) {
          belonging[parent2$belonging == group] = group
        }

        # some older groups may now be unselected
        group_lookup = group_lookup[groups %in% unique(c("1_1", belonging)), ]  # it may happen that "1_1" is empty
        group_lookup[, reordered := seq_len(.N)]
        stopifnot("1_1" %in% group_lookup$groups && group_lookup[groups == "1_1", ][["reordered"]] == 1)

        belonging = group_lookup$reordered[match(belonging, group_lookup$groups)]
        eqcs = unique(belonging)
        self$groups = list(features = parent1$features, eqcs = unique(c(1L, sort(eqcs))), belonging = belonging)

        # update group attributes (monotonicity)
        orig_eqcs = group_lookup$groups[match(eqcs, group_lookup$reordered)]
        all_monotone_eqcs = rbind(parent1_monotone_eqcs, parent2_monotone_eqcs)
        monotone_eqcs = all_monotone_eqcs[match(unique(c("1_1", orig_eqcs)), all_monotone_eqcs$eqcs), ]  # it may happen that "1_1" is empty
        monotone_eqcs[, eqcs := group_lookup$reordered[match(monotone_eqcs$eqcs, group_lookup$groups)]]
        setorderv(monotone_eqcs, "eqcs")
        self$monotone_eqcs = monotone_eqcs
      }, error = function(error_condition) {
        warning(error_condition$message)
        warning("Resetting due to error in crossover.")
        self$groups = old_groups
        self$monotone_eqcs = old_monotone_eqcs
      })

      invisible(NULL)
    },

    #' @description
    #' Calculates and returns the crossing sections for performing crossover between
    #' the group structures of two groups of [GroupStructure] objects. The crossing sections define the boundaries
    #' of groups to be exchanged between the parents during crossover.
    #'
    #' @param parent2 A `$groups` of a [GroupStructure] object representing the second parent.
    #'
    #' @return A list() containing the crossing sections for the first parent (`crossing_section1`) and second parent (`crossing_section2`).
    #'   Each crossing section is an integer vector of length 2, indicating the start and end points of the crossing section.
    get_crossing_sections = function(parent2) {
      # parent1 is self$groups
      # parent2 is $groups of another GroupStructure
      parent1 = self$groups
      parent2 = parent2$groups
      stopifnot(parent1$features == parent2$features)

      # some special handling needed if only one eqc and all features belong to it
      # eqc 1 is then empty (
      crossing_section1 = if (length(parent1$eqcs) == 1) c(1, 1) else sort(sample(seq_along(parent1$eqcs), size = 2L))  # AB |  C  | DE
      crossing_section2 = if (length(parent2$eqcs) == 1) c(1, 1) else sort(sample(seq_along(parent2$eqcs), size = 2L))  # a  | bcd | e

      list(crossing_section1 = crossing_section1, crossing_section2 = crossing_section2)
    }
  ),

  active = list(
    #' @field n_features (integer(1))\cr
    #'   Number of features part of the [mlr3::Task].
    n_features = function() {
      length(self$feature_names)
    },

    #' @field n_selected (integer(1))\cr
    #'   Number of selected features based on the group structure.
    n_selected = function() {
      length(self$selected_features)
    },

    #' @field selected_features (character())\cr
    #'   Names of the selected features.
    selected_features = function() {
      self$feature_names[self$groups$belonging != 1]
    },

    #' @field unselected_features (character())\cr
    #'   Names of the unselected features.
    unselected_features = function() {
      setdiff(self$feature_names, self$selected_features)
    },

    #' @field n_groups (integer(1))\cr
    #'   Number of groups under the group structure.
    #'   Determined based on the number of equivalence classes + 1 (due to the first group of unselected features also always being accounted for).
    n_groups = function() {
      length(self$groups$eqcs)  # first group is the unselected
    },

    #' @field monotone_features ([data.table::data.table])\cr
    #'   A data.table with as many rows as features with the feature column indicating the feature and the monotonicity column indicating the monotonicity constraint as an integer (`NA` indicating no constraint, e.g., for unselected features).
    monotone_features = function() {
      data.table(feature = self$groups$features, monotonicity = self$monotone_eqcs[self$groups$belonging]$monotonicity)
    },

    #' @field groups (list())\cr
    #'   A list containing a character vector of all features (`$features`), an integer vector of all equivalence classes (`$eqcs`) and an integer vector indicating the belonging of each feature to each class (`$belonging)`.
    groups = function(rhs) {
      if (!missing(rhs)) {
        assert_list(rhs, len = 3L, types = c("character", "integer", "integer"), any.missing = FALSE, names = "named")
        assert_true(all(names(rhs) == c("features", "eqcs", "belonging")))
        assert_set_equal(rhs[["features"]], self$feature_names)
        # normally there must be at least 2 groups: the unselected one and one selected
        # upper is p + 1 because we may have the empty unselected group + each feature being in its own
        # if self$.allow_all_unselected = TRUE, all can be unselected
        min_len = if (self$allow_all_unselected) 1L else 2L
        assert_integer(rhs[["eqcs"]], lower = 1L, upper = length(self$feature_names) + 1L, any.missing = FALSE, min.len = min_len, unique = TRUE)
        assert_true(all(rhs[["belonging"]] %in% rhs[["eqcs"]]))
        private$.groups = rhs
      } else {
        private$.groups
      }
    },

    #' @field monotone_eqcs ([data.table::data.table])\cr
    #'   A data.table with as many rows as equivalence classes with the eqcs column indicating the equivalence class and the monotonicity column indicating the monotonicity constraint as an integer (`NA` indicating no constraint, e.g., for the group of unselected features).
    monotone_eqcs = function(rhs) {
      if (!missing(rhs)) {
        assert_data_table(rhs, min.rows = 1L, max.rows = length(self$feature_names) + 1L, types = "integer")
        assert_true(rhs[1L, ][["eqcs"]] == 1L && is.na(rhs[1L, ][["monotonicity"]]))
        # normally there must be at least 2 groups: the unselected one and one selected
        # upper is p + 1 because we may have the empty unselected group + each feature being in its own
        # if self$.allow_all_unselected = TRUE, all can be unselected
        min_len = if (self$allow_all_unselected) 1L else 2L
        assert_integer(rhs[["eqcs"]], lower = 1L, upper = length(self$feature_names) + 1L, any.missing = FALSE, min.len = min_len, unique = TRUE)
        assert_true(all(rhs[["eqcs"]] == seq_len(nrow(rhs))))
        assert_integer(rhs[["monotonicity"]], lower = 0L, upper = 1L)
        private$.monotone_eqcs = rhs
      } else {
        private$.monotone_eqcs
      }
    },


    #' @field allow_all_unselected (logical(1))\cr
    #'   Whether unselecting all features should be allowed i.e., having only a single group of unselected features is considered a valid group structure.
    allow_all_unselected = function(rhs) {
      if (!missing(rhs)) {
        private$.allow_all_unselected = assert_flag(rhs)
      } else {
        private$.allow_all_unselected
      }
    }
  ),

  private = list(
    .groups = NULL,
    .monotone_eqcs = NULL,
    .allow_all_unselected = NULL
  )
)

