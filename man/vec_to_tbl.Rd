% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gc-ims.R
\name{vec_to_tbl}
\alias{vec_to_tbl}
\title{Convert a GCIMS vector to a tbl}
\usage{
vec_to_tbl(gcims_vec)
}
\arguments{
\item{gcims_vec}{gcims vector}
}
\value{
gcims tibble
}
\description{
Takes a GCIMS vector (vector with names giving the drift:retention times)
and returns a tbl with GC (retention) time, IMS (drift) time, and the value
given in the input vector.
}
