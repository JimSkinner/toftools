% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossvalidate.R
\name{crossvalidation_predictive_probabilities}
\alias{crossvalidation_predictive_probabilities}
\title{Crossvalidation predictive probabilities}
\usage{
crossvalidation_predictive_probabilities(classifier)
}
\arguments{
\item{classifier}{output from 'crossvalidate' function}
}
\description{
Given a classifier output by 'crossvalidate', return a tibble with one row
for each training sample containing the sample name, observed class,
predicted class, as well as the predicted probability of each class.
}
