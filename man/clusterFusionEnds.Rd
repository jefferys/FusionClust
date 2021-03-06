% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fusionClust.R
\name{clusterFusionEnds}
\alias{clusterFusionEnds}
\title{Cluster and annotate a fusion end list}
\usage{
clusterFusionEnds(ends, window = 10)
}
\arguments{
\item{ends}{A dataframe of fusion ends, sorted.}

\item{window}{The size of the clustering window, in base pairs. Two fusion
ends where the difference in the last sequenced base position is less than
this will be merged, e.g. if the positions are 10 and 19, a window of 10
will merge these, a window of 9 will not.}
}
\value{
The data frame of fusion ends, annotated with the cluster they
belong to (adding columns clusterId, clusterLow, and clusterHigh).
}
\description{
Cluster and annotate a fusion end list
}
