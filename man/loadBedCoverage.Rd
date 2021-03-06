% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fusionClust.R
\name{loadBedCoverage}
\alias{loadBedCoverage}
\title{Load a bed per-base coverage file}
\usage{
loadBedCoverage(file, merged = FALSE)
}
\arguments{
\item{file}{The file to load}

\item{merged}{TRUE if merged file from multiple samples with the first
column the "sample"}
}
\value{
A data frame with the coverage info, including a sample column
if a merged file.
}
\description{
Load a bed per-base coverage file
}
