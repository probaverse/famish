# Get all available methods for each distribution family by scraping
# the `methods` argument from each `fit_dst_<family>()` function.
# This means that the definitive source of available methods is through this
# family of functions.
available_methods <- function() {
  fit_dst_fams <- ls(getNamespace("famish"), pattern = "fit_dst_")
  fams <- gsub("fit_dst_", "", fit_dst_fams)
  methods <- lapply(fit_dst_fams, function(fam) {
    eval(formals(fam)[["method"]])
  })
  stats::setNames(methods, fams)
}