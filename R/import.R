#' Parse test output
#'
#' @param txt A \code{character} string such as this returned by \code{\link[base]{readLines}}
#'
#' @return A \code{data.frame}
#' @importFrom stringr str_split
#' @export
parse_test <- function(txt) {
  tst <- grep('^>>', txt, value = TRUE)
  tst <- sub('^>>', '', tst)
  tst <- str_split(tst[tst != 'END_MODULE'], '\t', simplify = TRUE)
  tst <- as.data.frame(tst, stringsAsFactors = FALSE)
  names(tst) <- c('name', 'output')
  return(tst)
}

#' Parse indecies for test data.frames
#'
#' @inheritParams parse_test
#' @param tst A \code{data.frame} with test output summary such as that returned by \link{parse_test}
#'
#' @return A \code{data.frame} with the indices of the tests
#' @export
parse_index <- function(txt, tst) {
  ind <- data.frame(name = tst$name,
                    start = unlist(lapply(tst$name, grep, x = txt)) + 1,
                    end = grep('END_MODULE', txt) - 1,
                    stringsAsFactors = FALSE)
  ind <- dplyr::filter(ind, (end - start) > 1)
  excp <- grep('Sequence Duplication Levels', ind$name)
  ind[excp, 'start'] <- ind$start[excp] + 1
  return(ind)
}

#' Parse test data.frames
#'
#' @inheritParams parse_test
#' @param start An \code{integer} to mark the beginning of the table for a test
#' @param end An \code{integer} to mark the end of the table for a test
#'
#' @return A \code{data.frame} of the test data.
#' @importFrom stringr str_split
#' @export
parse_data <- function(txt, start, end) {
  dat <- txt[start:end]
  dat <- str_split(dat, '\t', simplify = TRUE)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  col_names <- sub('^#', '', dat[1,])
  names(dat) <- col_names
  return(dat[-1,])
}

#' Parse fastQC file
#'
#' @param file_name A \code{character} refers to the text file from fastQC.
#'
#' @return A \code{list} containing a \code{data.frame} for each test
#' @export
parse_fqc <- function(file_name) {
  txt <- readLines(file_name)
  tst <- parse_test(txt)
  ind <- parse_index(txt, tst)
  ll <- list()
  for(i in seq_along(ind$name)) {
    ll[[i]] <- parse_data(txt, ind$start[i], ind$end[i])
  }
  return(ll)
}
