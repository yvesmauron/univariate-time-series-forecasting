% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_combination.R
\name{validation_split}
\alias{validation_split}
\title{Validation Split}
\usage{
validation_split(
  y,
  val_h,
  sov_only = F,
  val_min_years = 4,
  cv_min_years = 5,
  cv_max_samples = 3,
  ...
)
}
\arguments{
\item{y}{Time series onject.}

\item{val_h}{Horizon to be used for validation.}

\item{sov_only}{Flag, whether only single-origin validation should be considered. Optional, default \code{TRUE}.}

\item{val_min_years}{Minimum years required to conduct single origin validation. Optional, default \code{4}.}

\item{cv_min_years}{Minimum years required to conduct cross-origin validation. Optional, default \code{5}.}

\item{cv_max_samples}{Maximum samples that should be considered during cross-validation, i.e. 3 indicates the algorithm validates from 3 origins. Optional, default \code{3}.}

\item{...}{not used}
}
\value{
list of validation sets
}
\description{
This function splits a time series into validation sets with training and test data; the output can be used to backtest models and/or select/combine models based on how they perfrom on this sets.
}
