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
  } else if(test %in% c('Basic Statistics', 'summary')) {
    test_dat <- fqc[[test]]
  } else {
    test_dat <- fqc[[test]]
    test_dat[, 1] <- factor(test_dat[, 1],
                            levels = unique(test_dat[, 1]))
    mutate_at(test_dat,
              vars(2:ncol(test_dat)),
              function(x) as.numeric(x))
  }
}

#' Plot method for FastQC objects
#'
#' @inheritParams test_get
#' @param ... Other arguments to pass to \code{link[graphics]{plot}}
#'
#' @return A line graph.
#' @importFrom graphics lines axis
#' @export
plot <- function(fqc, test = NULL, ...) {
  UseMethod('plot')
}

#' @export
plot.FastQC <- function(fqc, test = NULL, ...) {
  if(is.null(test)) {
    stop('User Should provied a test name.')
  } else if(test == 'Basic Statistics') {
    stop('Nothing to plot. Use test_get instead.')
  } else if(test == 'summary') {
    stop('Nothing to plot. Use summary instead.')
  }
  tst_dat <- test_get(fqc, test)
  x <- tst_dat[, 1]
  if(test %in% c('Per base sequence content', 'Adapter Content')) {
    y <- tst_dat[, 2:ncol(tst_dat)]
  } else {
    y  <- tst_dat[, 2]
  }
  if(is.null(dim(y))) {
    plot(x, y, type = 'n', xaxt = 'n', main = test, ...)
    lines(x, y, ...)
    axis(1, at = 1:length(x), labels = x)
  } else if(ncol(y) > 1) {
    plot(x, y[, 1], type = 'n', xaxt = 'n', main = test, ...)
    axis(1, at = 1:length(x), labels = x)
    for(i in 1:ncol(y)) {
      lines(x, y = y[,i], ...)
    }
  }
}
