#' Extract data for a particular test
#'
#' Extract the data of a particular test from a \code{FastQC} or
#' \code{multiFasQC} object.
#'
#' @param fqc An object of class FastQC such as that returned by \link{FastQC}
#' or \link{multiFastQC}
#' @param test A \code{character} string of the test name.
#'
#' @return A \code{data.frame} of the test data.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' fqc <- parse_fqc(fl)
#' fqc <- FastQC(fqc)
#' tst_dat <- test_get(fqc, test = 'Per base sequence quality')
#'
#' @importFrom dplyr mutate_at vars
#'
#' @export
test_get <- function(fqc, test = NULL) {
    # the critical factor is the test in query.
    # a NULL test returns an error
    # Basic Statistics and summaries are items in the object list
    # so the function returns them as is
    # processing other tests involve:
    if(is.null(test)) {
        stop('User should provide a test name.')
    } else if(test %in% c('Basic Statistics', 'summary', 'Kmer Content')) {
        test_dat <- fqc[[test]]
        return(test_dat)
    } else {
        # subset test data.frame
        test_dat <- fqc[[test]]

        # first column as a factor
        test_dat[, 1] <- factor(test_dat[, 1],
                                levels = unique(test_dat[, 1]))

        if(class(fqc) == 'FastQC') {
            # other columns as numeric
            mutate_at(test_dat,
                      vars(2:ncol(test_dat)),
                      function(x) as.numeric(x))
        } else {
            # when input object is a multiFastQC:
            # spare the file column from the numeric conversion
            mutate_at(test_dat,
                      vars(2:ncol(test_dat), -file),
                      function(x) as.numeric(x))
        }
    }
}


#' Plot test data
#'
#' The function generates line graphs of the different test data. Mainly, its
#' a wrapper around the base R \code{\link[graphics]{plot}} to handle the
#' different types of tests.
#'
#' @param tst_dat A \code{data.frame} containing data for a single test.
#' @inheritParams test_get
#' @param color A \code{character} vector of length equals to the number of
#' lines to be plotted.
#' @param ... Other arguments passed to the \code{\link[graphics]{plot}}
#' function.
#'
#' @return A line graph
#'
#' @export
test_plot <- function(tst_dat, test, color = 'black', ...) {
    x <- tst_dat[, 1]
    if(test %in% c('Per base sequence content',
                   'Sequence Duplication Levels',
                   'Adapter Content')) {
        y <- tst_dat[, 2:ncol(tst_dat)]
        if(length(color) != ncol(y)) {
            warning(paste('color is not a vector of length ',
                          ncol(y),
                          '. Using default colors.',
                          sep = ''))
            color <- c('red', 'blue', 'green', 'orange')
            }
    } else {
        y  <- tst_dat[, 2]
    }
    if(is.null(dim(y))) {
        plot(x, y, type = 'n', xaxt = 'n', ...)
        lines(x, y, col = color, ...)
        axis(1, at = 1:length(x), labels = x)
    } else if(ncol(y) > 1) {
        plot(x, y[, 1], type = 'n', xaxt = 'n', ...)
        axis(1, at = 1:length(x), labels = x)
        for(i in 1:ncol(y)) {
            lines(x, y = y[,i], col = color[i], ...)
        }
    }
}

#' Summary method for FastQC/multiFastQC objects
#'
#' @inheritParams test_get
#'
#' @return Prints summary of test data
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' fqc <- parse_fqc(fl)
#' fqc <- FastQC(fqc)
#' summary(fqc)
#'
#' @importFrom tidyr spread
#'
#' @export
summary <- function(fqc){
  UseMethod('summary')
}

#' @export
summary.FastQC <- function(fqc) {
  summ <- test_get(fqc, 'summary')
  print(summ)
}

#' @export
summary.multiFastQC <- function(fqc) {
  summ <- test_get(fqc, 'summary')
  summ <- spread(summ, file, output)
  print(summ)
}

#' Plot method for FastQC/multiFastQC objects
#'
#' @inheritParams test_get
#' @inheritParams test_plot
#' @param ... Other arguments to pass to \code{\link[graphics]{plot}}
#'
#' @return A line graph.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' fqc <- parse_fqc(fl)
#' fqc <- FastQC(fqc)
#' plot(fqc, test = 'Per base sequence quality')
#'
#' @importFrom graphics lines axis mtext
#'
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
  test_plot(tst_dat, test, main = test, ...)
}

#' @export
plot.multiFastQC <- function(fqc, test = NULL, ...) {
  tst <- test_get(fqc, test)
  tst <- split(tst[, -ncol(tst)], tst$file)
  nms <- names(tst)
  for(i in seq_along(nms)) {
    test_plot(tst[[i]], test, main = test, ...)
    mtext(nms[i], line = .5)
  }
}
