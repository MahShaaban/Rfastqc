#' \code{Rfastqc} package
#'
#' Parse and manage the test data from the fastQC text output
#'
#' @docType package
#' @name Rfastqc
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## fix by @jennybc
## source https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c('end',
                                                        'start',
                                                        'output'))
