% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_accuracy.R
\name{smase}
\alias{smase}
\title{Seasonal Mean absolut scaled error (sMASE)}
\usage{
smase(y_true, y_pred, in_sample, frequency, ...)
}
\arguments{
\item{y_true}{actual value of time series}

\item{y_pred}{predicted value of time series}

\item{in_sample}{in sample (hystorical values) of time series}

\item{frequency}{frequency of time series}
}
\value{
Returns the mean of the absolute differences scaled by the mean in-sample error of seasonal naive
}
\description{
Seasonal Mean absolut scaled error (sMASE)
}
