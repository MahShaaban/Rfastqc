% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary}
\alias{summary}
\title{Summary method for FastQC/multiFastQC objects}
\usage{
summary(fqc)
}
\arguments{
\item{fqc}{An object of class FastQC such as that returned by \link{FastQC}
or \link{multiFastQC}}
}
\value{
Prints summary of test data
}
\description{
Summary method for FastQC/multiFastQC objects
}
\examples{
fl <- system.file("extdata", "sample_1.txt", package = "Rfastqc")
fqc <- parse_fqc(fl)
fqc <- FastQC(fqc)
summary(fqc)

}
