#' @keywords internal 
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib BrainGnomes, .registration = TRUE
## usethis namespace: end
NULL

# Add globals to avoid R CMD check complaint
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(":=", "resample_template_to_bold"))
}
