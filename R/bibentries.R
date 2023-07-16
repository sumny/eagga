# nocov start
format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  lou_2013    = bibentry("inproceedings",
    title     = "Accurate Intelligible Models with Pairwise Interactions",
    author    = "Lou, Yin and Caruana, Rich and Gehrke, Johannes and Hooker, Giles",
    booktitle = "Proceedings of the 19th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining",
    pages     = "623--631",
    year      = "2013"
  ),
  schneider_2023 = bibentry("inproceedings",
    title     = "Multi-Objective Optimization of Performance and Interpretability of Tabular Supervised Machine Learning Models",
    author    = "Schneider, Lennart and Bischl, Bernd and Thomas, Janek",
    year      = "2023",
    url       = "https://doi.org/10.1145/3583131.3590380",
    doi       = "10.1145/3583131.3590380",
    booktitle = "Proceedings of the Genetic and Evolutionary Computation Conference",
    pages     = "538–547",
    series    = "GECCO '23"
  )
)
# nocov end
