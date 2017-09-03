#' Construct FastQC object
#'
#' @param fqc A \code{list} of test data such as that returned by \link{parse_fqc}
#'
#' @return An object of class FastQC
#' @export
FastQC <- function(fqc) {
  structure(fqc, class = 'FastQC')
}

#' Construc multiFastQC object
#'
#' @inheritParams FastQC
#'
#' @return An object of class multiFastQC
#' @importFrom dplyr bind_rows select
#' @export
multiFastQC <- function(fqc) {
  nms <- unique(unlist(lapply(fqc, function(x) names(x))))
  test_dat <- list()
  for(i in seq_along(nms)){
    test_dat[[i]] <- lapply(fqc, function(x) x[[nms[i]]])
  }
  names(test_dat) <- nms
  test_dat <- lapply(test_dat, bind_rows, .id = 'file')
  test_dat <- lapply(test_dat, function(x) select(x, 2:ncol(x), 1))
  structure(test_dat, class = 'multiFastQC')
}

