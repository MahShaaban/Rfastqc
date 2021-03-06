% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{parse_collection}
\alias{parse_collection}
\title{Parse fastQC collection of files}
\usage{
parse_collection(file_names, names = NULL)
}
\arguments{
\item{file_names}{A \code{character} vector of file names}

\item{names}{A \code{character} vector of length equals that of file_names}
}
\value{
A \code{list} os lists. The outer list is named by the file names or
the input of the argument \code{names} of equivelant length.
}
\description{
A wrapper function to full parse multiple output text files of the FastQC
utility. The function uses the \link{parse_fqc} to parse each file separatly
in a named \code{list}.
}
\examples{
path <- system.file('extdata', package = 'Rfastqc')
fls <- list.files(path, full.names = TRUE)
fqc <- parse_collection(fls)

}
