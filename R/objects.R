#' Construct FastQC object
#'
#' @param fqc A \code{list} of test data such as that returned by \link{parse_fqc}
#'
#' @return An object of class FastQC
#' @export
FastQC <- function(fqc) {
  structure(fqc, class = 'FastQC')
}
