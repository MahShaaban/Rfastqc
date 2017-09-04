#' Construct FastQC object
#'
#' @param fqc A \code{list} of test data such as that returned by \link{parse_fqc}
#'
#' @return An object of class \code{FastQC}
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' fqc <- parse_fqc(fl)
#' fqc_ob <- FastQC(fqc)
#'
#' @export
FastQC <- function(fqc) {
  structure(fqc, class = 'FastQC')
}

#' Construct multiFastQC object
#'
#' @inheritParams FastQC
#'
#' @return An object of class \code{multiFastQC}
#'
#' @examples
#' path <- system.file('extdata', package = 'Rfastqc')
#' fls <- list.files(path, full.names = TRUE)
#' fqc <- parse_collection(fls)
#' fqc_ob <- multiFastQC(fqc)
#'
#' @importFrom dplyr bind_rows select
#'
#' @export
multiFastQC <- function(fqc) {
    # get unique names of test in the list
    nms <- unique(unlist(lapply(fqc, names)))

    # make a list of lists of data.frames
    test_dat <- list()
    for(i in seq_along(nms)){
        test_dat[[i]] <- lapply(fqc, function(x) x[[nms[i]]])
        }
    names(test_dat) <- nms

    # bind rows of test data.frames in the list and reorder
    test_dat <- lapply(test_dat, bind_rows, .id = 'file')
    test_dat <- lapply(test_dat, function(x) select(x, 2:ncol(x), 1))

    structure(test_dat, class = 'multiFastQC')
}

