library(data.table)
library(mlr3misc)
library(bbotk)
library(ggplot2)
library(emoa)
library(pammtools)
library(scmamp)
library(xtable)

# Section 5.2 and 5.3
eagga = readRDS("results/eagga_ours_so.rds")[method %in% c("eagga")]
eagga_dhv = map_dtr(seq_len(nrow(eagga)), function(i) {
  x = eagga[i, ]
  tmp = x$dhv_val_anytime[[1L]]
  tmp[, task_id := x$task_id]
  tmp[, method := x$method]
  tmp[, repl := x$repl]
  tmp
})

xgboost_mo = readRDS("results/eagga_ablation_xgboost_mo.rds")
xgboost_mo_dhv = map_dtr(seq_len(nrow(xgboost_mo)), function(i) {
  x = xgboost_mo[i, ]
  tmp = x$dhv_val_anytime[[1L]]
  tmp[, task_id := x$task_id]
  tmp[, method := x$method]
  tmp[, repl := x$repl]
  tmp
})

eagga_ablation = readRDS("results/eagga_ablation_eagga.rds")
eagga_ablation_dhv = map_dtr(seq_len(nrow(eagga_ablation)), function(i) {
  x = eagga_ablation[i, ]
  tmp = x$dhv_val_anytime[[1L]]
  tmp[, task_id := x$task_id]
  tmp[, method := x$method]
  tmp[, repl := x$repl]
  tmp[, crossover := x$crossover]
  tmp[, mutation := x$mutation]
  tmp[, detectors := x$detectors]
  tmp[, random := x$random]
  tmp
})
eagga_ablation_dhv[, method := paste0(crossover, "_", mutation, "_", detectors, "_", random)]

anytime_hvs = rbind(eagga_dhv, eagga_ablation_dhv, xgboost_mo_dhv, fill = TRUE)

anytime_hvs_runtime = map_dtr(unique(anytime_hvs$task_id), function(task_id_) {
  seqs = seq(0, 8L * 3600L, length.out = 1001L)
  ref = anytime_hvs[task_id == task_id_]
  min_ = min(ref$dhv_val)
  max_ = max(ref$dhv_val)
  anytime_hvs[task_id == task_id_, dhv_val_normalized := (dhv_val - min_) / (max_ - min_)]
  map_dtr(unique(anytime_hvs$repl), function(repl_) {
    tmp = anytime_hvs[task_id == task_id_ & repl == repl_]
    result = map_dtr(seqs, function(seq_) {
      tmp[runtime <= seq_, .(dhv_val = max(dhv_val), dhv_val_normalized = max(dhv_val_normalized), runtime = seq_), by = .(method)]
    })
    result[, task_id := task_id_]
    result[, repl := repl_]
    result
  })
})

anytime_hvs_init = map_dtr(unique(anytime_hvs$task_id), function(task_id_) {
  map_dtr(unique(anytime_hvs$repl), function(repl_) {
    tmp_eagga = anytime_hvs[method != "xgboost_mo" & task_id == task_id_ & repl == repl_ & iteration == 101L]  # mu = 100
    tmp_xgboost_mo = anytime_hvs[method == "xgboost_mo" & task_id == task_id_ & repl == repl_ & iteration == 41L]  # xgboost search space d = 10, init 4 * d
    rbind(tmp_eagga, tmp_xgboost_mo)
  })
})

mean_anytime_hvs_runtime = anytime_hvs_runtime[, .(mean_anytime_hv = mean(dhv_val), se_anytime_hv = sd(dhv_val) / sqrt(.N), mean_anytime_hv_normalized = mean(dhv_val_normalized), se_anytime_hv_normalized = sd(dhv_val_normalized) / sqrt(.N)), by = .(runtime, method, task_id)]
mean_anytime_hvs_init = anytime_hvs_init[, .(mean_init = mean(runtime), se_init = sd(runtime) / sqrt(.N)), by = .(method, task_id)]

mean_anytime_hvs_runtime[, method := factor(method, labels = c("EAGGA_XGBoost", "No_Crossover", "No_Mutation", "No_Cross_Mut", "No_Detectors", "Random Search", "XGBoost_MO"))]
mean_anytime_hvs_init[, method := factor(method, labels = c("EAGGA_XGBoost", "No_Crossover", "No_Mutation", "No_Cross_Mut", "No_Detectors", "Random Search", "XGBoost_MO"))]

# Figure 4 xgboost_mo anytime valid and test
mean_anytime_hvs_runtime_xgboost = mean_anytime_hvs_runtime[method %in% c("EAGGA_XGBoost", "XGBoost_MO")]
mean_anytime_hvs_init_xgboost = mean_anytime_hvs_init[method %in% c("EAGGA_XGBoost", "XGBoost_MO")]

g = ggplot(aes(x = runtime, y = mean_anytime_hv, colour = method, fill = method), data = mean_anytime_hvs_runtime_xgboost) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.1) +
  labs(y = "Mean Dominated Hypervolume", x = "Runtime (s)", colour = "Method", fill = "Method", linetype = "Method") +
  scale_x_log10() +
  facet_wrap(~ task_id, scales = "free", nrow = 5, ncol = 4) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("plots/eagga_ablation_xgboost.pdf", plot = g, device = "pdf", width = 10, height = 8)

wilcoxonSignedTest(mean_anytime_hvs_runtime_xgboost[method == "EAGGA_XGBoost" & runtime == 8L * 3600L, ]$mean_anytime_hv, mean_anytime_hvs_runtime_xgboost[method == "XGBoost_MO" & runtime == 8L * 3600L, ]$mean_anytime_hv) # T = 30, p = 0.002556

# xgboost_mo final test
all = readRDS("results/eagga_ours_so.rds")
dat = rbind(all, xgboost_mo, fill = TRUE)
majority = readRDS("results/eagga_majority_vote.rds")

ref = t(t(c(minus_classif.auc = 0, selected_features = 1, selected_interactions = 1, selected_non_monotone = 1)))
fct = c(-1, 1, 1, 1)
ys = c("auc_test", "selected_features_proxy", "selected_interactions_proxy", "selected_non_monotone_proxy")

hvs = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (!all(c("eagga", "eagga_md2", "xgboost_mo") %in% tmp$method)) {
      return(data.table())
    }
    eagga = tmp[method == "eagga"]$pareto[[1L]][, ..ys]
    eagga = rbind(eagga, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_hv = dominated_hypervolume(t(eagga) * fct, ref = ref)
    eagga_md2 = tmp[method == "eagga_md2"]$pareto[[1L]][, ..ys]
    eagga_md2 = rbind(eagga_md2, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_md2_hv = dominated_hypervolume(t(eagga_md2) * fct, ref = ref)
    xgboost_mo = tmp[method == "xgboost_mo"]$pareto[[1L]][, ..ys]
    xgboost_mo = rbind(xgboost_mo, majority[task_id == task_id_ & repl == repl_, ..ys])
    xgboost_mo_hv = dominated_hypervolume(t(xgboost_mo) * fct, ref = ref)
    data.table(eagga_hv = eagga_hv, eagga_md2_hv = eagga_md2_hv, xgboost_mo_hv = xgboost_mo_hv, repl = repl_, task_id = task_id_)
  })
})

mean_hvs = hvs[, .(mean_eagga_hv = mean(eagga_hv), se_eagga_hv = sd(eagga_hv) / sqrt(.N), mean_eagga_md2_hv = mean(eagga_md2_hv), se_eagga_md2_hv = sd(eagga_md2_hv) / sqrt(.N), mean_xgboost_mo_hv = mean(xgboost_mo_hv), se_xgboost_mo_hv = sd(xgboost_mo_hv) / sqrt(.N)), by = .(task_id)]

mean_hvs = rbind(data.table(task_id = mean_hvs$task_id, mean_hv = mean_hvs$mean_eagga_hv, se_hv = mean_hvs$se_eagga_hv, method = "EAGGA_XGBoost"),
                 data.table(task_id = mean_hvs$task_id, mean_hv = mean_hvs$mean_eagga_md2_hv, se_hv = mean_hvs$se_eagga_md2_hv, method = "EAGGA_XGBoost_md2"),
                 data.table(task_id = mean_hvs$task_id, mean_hv = mean_hvs$mean_xgboost_mo_hv, se_hv = mean_hvs$se_xgboost_mo_hv, method = "XGBoost_MO"))
mean_hvs[, task_id := as.factor(task_id)]
mean_hvs[, method := factor(method, levels = c("XGBoost_MO", "EAGGA_XGBoost", "EAGGA_XGBoost_md2"))]

# Figure 2 and test
g = ggplot(aes(x = task_id, y = mean_hv, colour = method), data = mean_hvs) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_hv - se_hv, ymax = mean_hv + se_hv), width = 0.5, position = position_dodge(width = 0.5)) +
  labs(y = "Mean Dominated Hypervolume", x = "Task ID", colour = "Method") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60), legend.position = "bottom")

ggsave("plots/mdhv_xgboost.pdf", plot = g, device = "pdf", width = 6, height = 4)

wilcoxonSignedTest(mean_hvs[method == "EAGGA_XGBoost", ]$mean_hv, mean_hvs[method == "XGBoost_MO", ]$mean_hv) # T = 40, p = 0.00762
wilcoxonSignedTest(mean_hvs[method == "EAGGA_XGBoost_md2", ]$mean_hv, mean_hvs[method == "XGBoost_MO", ]$mean_hv) # T = 50, p = 0.02002

# Mind the Gap
# doesn't change much
get_pessimistic_front = function(front) {
  front = fct * t(front)
  ids = map_lgl(seq_len(ncol(front)), function(j) {
    tmp = map_lgl(seq_len(ncol(front)), function(k) {
      emoa::is_dominated(front[, c(j, k)])[2L]
    })
    all(!tmp)
  })
  t(fct * front[, ids])
}

hvs_gap = map_dtr(unique(dat$task_id), function(task_id_) {
  map_dtr(unique(dat$repl), function(repl_) {
    tmp = dat[task_id == task_id_ & repl == repl_]
    if (!all(c("eagga", "eagga_md2", "xgboost_mo") %in% tmp$method)) {
      return(data.table())
    }
    eagga = tmp[method == "eagga"]$pareto[[1L]][, ..ys]
    eagga = rbind(eagga, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_hv = dominated_hypervolume(t(eagga) * fct, ref = ref)

    eagga_pess = get_pessimistic_front(tmp[method == "eagga"]$pareto[[1L]][, ..ys])
    eagga_pess = rbind(eagga_pess, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_pess_hv = dominated_hypervolume(t(eagga_pess) * fct, ref = ref)

    eagga_md2 = tmp[method == "eagga_md2"]$pareto[[1L]][, ..ys]
    eagga_md2 = rbind(eagga_md2, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_md2_hv = dominated_hypervolume(t(eagga_md2) * fct, ref = ref)

    eagga_md2_pess = get_pessimistic_front(tmp[method == "eagga_md2"]$pareto[[1L]][, ..ys])
    eagga_md2_pess = rbind(eagga_md2_pess, majority[task_id == task_id_ & repl == repl_, ..ys])
    eagga_md2_pess_hv = dominated_hypervolume(t(eagga_md2_pess) * fct, ref = ref)

    xgboost_mo = tmp[method == "xgboost_mo"]$pareto[[1L]][, ..ys]
    xgboost_mo = rbind(xgboost_mo, majority[task_id == task_id_ & repl == repl_, ..ys])
    xgboost_mo_hv = dominated_hypervolume(t(xgboost_mo) * fct, ref = ref)

    xgboost_mo_pess = get_pessimistic_front(tmp[method == "xgboost_mo"]$pareto[[1L]][, ..ys])
    xgboost_mo_pess = rbind(xgboost_mo_pess, majority[task_id == task_id_ & repl == repl_, ..ys])
    xgboost_mo_pess_hv = dominated_hypervolume(t(xgboost_mo_pess) * fct, ref = ref)

    data.table(eagga_hv = eagga_hv,
               eagga_pess_hv = eagga_pess_hv,
               eagga_gap = eagga_hv - eagga_pess_hv,
               eagga_md2_hv = eagga_md2_hv,
               eagga_md2_pess_hv = eagga_md2_pess_hv,
               eagga_md2_gap = eagga_md2_hv - eagga_md2_pess_hv,
               xgboost_mo_hv = xgboost_mo_hv,
               xgboost_mo_pess_hv = xgboost_mo_pess_hv,
               xgboost_mo_gap = xgboost_mo_hv - xgboost_mo_pess_hv,
               repl = repl_,
               task_id = task_id_)
  })
})

mean_hvs_gap = hvs_gap[, .(mean_eagga_gap = mean(eagga_gap), se_eagga_gap = sd(eagga_gap) / sqrt(.N), mean_eagga_md2_gap = mean(eagga_md2_gap), se_eagga_md2_gap = sd(eagga_md2_gap) / sqrt(.N), mean_xgboost_mo_gap = mean(xgboost_mo_gap), se_xgboost_mo_gap = sd(xgboost_mo_gap) / sqrt(.N)), by = .(task_id)]

mean_hvs_gap = rbind(data.table(task_id = mean_hvs_gap$task_id, mean_hv_gap = mean_hvs_gap$mean_eagga_gap, se_hv_gap = mean_hvs_gap$se_eagga_gap, method = "EAGGA_XGBoost"),
                     data.table(task_id = mean_hvs_gap$task_id, mean_hv_gap = mean_hvs_gap$mean_eagga_md2_gap, se_hv_gap = mean_hvs_gap$se_eagga_md2_gap, method = "EAGGA_XGBoost_md2"),
                     data.table(task_id = mean_hvs_gap$task_id, mean_hv_gap = mean_hvs_gap$mean_xgboost_mo_gap, se_hv_gap = mean_hvs_gap$se_xgboost_mo_gap, method = "XGBoost_MO"))
mean_hvs_gap[, task_id := as.factor(task_id)]
mean_hvs_gap[, method := factor(method, levels = c("EAGGA_XGBoost", "EAGGA_XGBoost_md2", "XGBoost_MO"))]

g = ggplot(aes(x = task_id, y = mean_hv_gap, colour = method), data = mean_hvs_gap) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_hv_gap - se_hv_gap, ymax = mean_hv_gap + se_hv_gap), width = 0.5, position = position_dodge(width = 0.5)) +
  labs(y = "Mean Hypervolume Gap", x = "Task ID", colour = "Method") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60), legend.position = "bottom")

gap_winner = hvs_gap[, .(eagga_winner = eagga_pess_hv > xgboost_mo_hv, eagga_md2_winner = eagga_md2_pess_hv > xgboost_mo_hv, xgboost_mo_winner = xgboost_mo_pess_hv > eagga_hv), by = .(repl, task_id)]
mean_gap_winner = gap_winner[, .(mean_eagga_winner = mean(eagga_winner), se_eagga_winner = sd(eagga_winner) / sqrt(.N),
                                 mean_eagga_md2_winner = mean(eagga_md2_winner), se_eagga_md2_winner = sd(eagga_md2_winner) / sqrt(.N),
                                 mean_xgboost_mo_winner = mean(xgboost_mo_winner), se_xgboost_mo_winner = sd(xgboost_mo_winner) / sqrt(.N)), by = .(task_id)]

# eagga ablation anytime valid

# Figure 5
g = ggplot(aes(x = runtime, y = mean_anytime_hv, colour = method, fill = method), data = mean_anytime_hvs_runtime[method %nin% c("XGBoost_MO")]) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_anytime_hv - se_anytime_hv, ymax = mean_anytime_hv + se_anytime_hv), colour = NA, alpha = 0.1) +
  labs(y = "Mean Dominated Hypervolume", x = "Runtime (s)", colour = "Method", fill = "Method", linetype = "Method") +
  scale_x_log10() +
  facet_wrap(~ task_id, scales = "free", nrow = 5, ncol = 4) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("plots/eagga_ablation.pdf", plot = g, device = "pdf", width = 10, height = 8)

# Table 7
table_dat = mean_anytime_hvs_runtime[runtime == 8L * 3600L]

table_mean = xtabs(mean_anytime_hv ~ task_id + method, data = table_dat)
table_se = xtabs(se_anytime_hv ~ task_id + method, data = table_dat)

table = map_dtr(seq_len(nrow(table_mean)), function(i) {
  tmp_mean = table_mean[i, ]
  tmp_se = table_se[i, ]
  best = which.max(tmp_mean)
  x = paste0(sprintf(round(tmp_mean, 3), fmt = "%#.3f"), " (", sprintf(round(tmp_se, 3), fmt = "%#.3f"), ")")
  x[best] = paste0("\\textbf{", sprintf(round(tmp_mean[best], 3), fmt = "%#.3f"), "}", " (", sprintf(round(tmp_se[best], 3), fmt = "%#.3f"), ")")
  x = as.list(x)
  names(x) = colnames(table_mean)
  x
})
table = cbind(data.table(ID = rownames(table_mean)), table)
rownames(table) = NULL

table = xtable(table)
print(table, sanitize.text.function = function(x) x, include.rownames = FALSE)

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# test and cd plot Figure 3
tmp = as.matrix(dcast(mean_anytime_hvs_runtime[runtime == 8L * 3600L], task_id ~ method, value.var = "mean_anytime_hv")[, -1L])
friedmanTest(tmp)  # Friedman's chi-squared = 52.993, df = 6, p-value = 1.177e-09
pdf(file = "plots/eagga_ablation_cd_1.pdf", width = 10, height = 5)
plotCD(tmp, cex = 1)
dev.off()

