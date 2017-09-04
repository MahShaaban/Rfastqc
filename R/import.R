#' Parse test output
#'
#' The function parses the different test names and output in terms of pass or
#' fail from the text file output of the FastQC utility. Baically, the
#' function identies the test names lines' preceeded by '>>', parses the text
#' name and the output of that test in a \code{data.frame}.
#'
#' @param txt A \code{character} string such as this returned by
#' \code{\link[base]{readLines}}
#'
#' @return A \code{data.frame} of two columns \code{name} and \code{output}
#' fot the test name and output in terms of pass or fail.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' txt <- readLines(fl)
#' tst <- parse_test(txt)
#'
#' @importFrom stringr str_split
#'
#' @export
parse_test <- function(txt) {
    # identify the test name lines and remove prefix
    tst <- grep('^>>',
                x = txt,
                value = TRUE)
    tst <- sub('^>>',
              replacement = '',
              x = tst)

    # split the test line text
    tst <- str_split(tst[tst != 'END_MODULE'],
                   pattern = '\t',
                   simplify = TRUE)
    # make a data.frame with colnames name and ouput
    tst <- as.data.frame(tst,
                       stringsAsFactors = FALSE)
    names(tst) <- c('name', 'output')

    return(tst)
}

#' Parse indecies for test data.frames
#'
#' The function parces the indecies for the data.frames of a particular test.
#' Basically, the function uses the test names to identify the block of text
#' containing the relevant data. And identify the beginning and the end line
#' numbers of the test block.
#'
#' @inheritParams parse_test
#' @param tst A \code{data.frame} with test output summary such as that
#' returned by \link{parse_test}
#'
#' @return A \code{data.frame} with three columns; \code{name}, \code{start}
#' and \code{end} for the test name, start and end line number respectively.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' txt <- readLines(fl)
#' tst <- parse_test(txt)
#' ind <- parse_index(txt, tst)
#'
#' @importFrom dplyr filter
#'
#' @export
parse_index <- function(txt, tst) {
    # make a data.frame of test name, start and end
    # name: from parse_text
    # start: from grep test name on the text and adding 1
    # end: from grep the end 'END_MODULE' phrase and subsetting 1
    ind <- data.frame(name = tst$name,
                    start = unlist(lapply(tst$name, grep, x = txt)) + 1,
                    end = grep('END_MODULE', txt) - 1,
                    stringsAsFactors = FALSE)
    # handel exceptions:
    # 1) tests with one line of data
    # 2) Sequence Duplication Levels with an extra line in the header
    ind <- filter(ind, (end - start) > 1)
    excp <- grep('Sequence Duplication Levels', ind$name)
    ind[excp, 'start'] <- ind$start[excp] + 1

    return(ind)
}

#' Parse test data.frames
#'
#' The function parses the block of text of a particular test and returns a
#' tidy data.frame. The block of relevant text is first identified by a start
#' and and end line numbers and parsed as strings which is split later and
#' made a data.frame of.
#'
#' @inheritParams parse_test
#' @param start An \code{integer} to mark the beginning of the table for a test
#' @param end An \code{integer} to mark the end of the table for a test
#'
#' @return A \code{data.frame} of the test data.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' txt <- readLines(fl)
#' tst <- parse_test(txt)
#' ind <- parse_index(txt, tst)
#' dat <- parse_data(txt, start = ind$start[1], ind$end[1])
#'
#' @importFrom stringr str_split
#'
#' @export
parse_data <- function(txt, start, end) {
    # parse the blocks of text and split strings
    dat <- txt[start:end]
    dat <- str_split(dat,
                    pattern = '\t',
                    simplify = TRUE)
    # make data.frames and name columns
    dat <- as.data.frame(dat,
                        stringsAsFactors = FALSE)
    col_names <- sub('^#', '', dat[1,])
    names(dat) <- col_names

    return(dat[-1,])
}

#' Parse fastQC file
#'
#' A wrapper function to fully parse the text file output of FastQC in tidy
#' data.frames. Breifly, the function reads the text lines, parse the test
#' names, parse the test blocks of text and returns a list.
#'
#'
#' @param file_name A \code{character} refers to the text file from fastQC.
#'
#' @return A named \code{list} containing a \code{data.frame} for each test.
#'
#' @examples
#' fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
#' fqc <- parse_fqc(fl)
#'
#' @export
parse_fqc <- function(file_name) {
    # parse text, test blocks and indecies
    txt <- readLines(file_name)
    tst <- parse_test(txt)
    ind <- parse_index(txt, tst)

    # make a list of data.frames
    ll <- list()
    for(i in seq_along(ind$name)) {
        ll[[i]] <- parse_data(txt, ind$start[i], ind$end[i])
    }
    names(ll) <- ind$name

    # add summary containind the test data.frame
    ll$summary <- tst

    return(ll)
}

#' Parse fastQC collection of files
#'
#' A wrapper function to full parse multiple output text files of the FastQC
#' utility. The function uses the \link{parse_fqc} to parse each file separatly
#' in a named \code{list}.
#'
#' @param file_names A \code{character} vector of file names
#' @param names A \code{character} vector of length equals that of file_names
#'
#' @return A \code{list} os lists. The outer list is named by the file names or
#' the input of the argument \code{names} of equivelant length.
#'
#' @examples
#' path <- system.file('extdata', package = 'Rfastqc')
#' fls <- list.files(path, full.names = TRUE)
#' fqc <- parse_collection(fls)
#'
#' @export
parse_collection <- function(file_names, names = NULL) {
    # parse individual files in a list
    coll <- lapply(file_names, parse_fqc)

    # name the list
    if(is.null(names)) {
        names(coll) <- file_names
    } else {
        names(coll) <- names
    }

    return(coll)
}
