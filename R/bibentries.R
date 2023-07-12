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
  )
)
