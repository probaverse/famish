# Check if a method is supported for a family.
# Checks the `fit_dst_<family>()` functions.
# Also allows:
# - Cauchy family fit by mme or lmom: these methods just always fail
#   because Cauchy distributions have no defined moments, but are still
#   technically "supported" because we *know* what do.
# - Null: always supported because always returns a Null distribution
supported_combination <- function(family, method) {
  avail_methods <- available_methods()
  fam_methods <- avail_methods[[family]]
  if (is.null(fam_methods)) {
    return(FALSE)
  }
  if (family == "cauchy" && method %in% c("mme", "lmom")) {
    return(TRUE)
  }
  if (family %in% c("null", "finite", "empirical")) {
    return(TRUE)
  }
  return(method %in% fam_methods)
}