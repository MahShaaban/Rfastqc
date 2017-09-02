#' Summary method for FastQC objects
#'
#' @param fqc An object of class FastQC such as that returned by \link{FastQC}
#'
#' @return Prints summary of test data
#' @export
summary <- function(fqc){
  UseMethod('summary')
}

#' @export
summary.FastQC <- function(fqc) {
  print(fqc$summary)
}

#' Extract data for a particular test
#'
#' @inheritParams summary
#' @param test A \code{character} string of the test name
#'
#' @return A \code{data.frame} of the test data
#' @importFrom dplyr mutate_at
#' @export
test_get <- function(fqc, test = NULL) {
  if(is.null(test)) {
    stop('User should provide a test name')
  }
  if(class(fqc) != 'FastQC') {
    warning('User should a differen object. Will coerce to Fastqc')
    class(fqc) <- 'FastQC'
  }
  test_dat <- fqc[[test]]
  test_dat[, 1] <- factor(test_dat[, 1],
                          levels = unique(test_dat[, 1]))
  mutate_at(test_dat,
            vars(2:ncol(test_dat)),
            function(x) as.numeric(x))
}
