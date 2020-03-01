

# Absolute error measures --------------------------------------------------------------

#' Absolut error (AE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns a vector of absolute differences
#'
#' @export
ae <- function(y_true, y_pred, ...) {
  return(abs(y_true - y_pred))
}

#' Squared error (SE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns a vector of saured differences
#'
#' @export
se <- function(y_true, y_pred, ...) {
  return((y_true - y_pred) ^ 2)
}

#' Mean absolute error (MAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns ean of absolute differences
#'
#' @export
mae <- function(y_true, y_pred, ...) {
  return(mean(ae(y_true = y_true, y_pred = y_pred)))
}

#' Mean square error (MSE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns mean of squared errors
#'
#' @export
mse <- function(y_true, y_pred, ...) {
  return(mean(se(y_true = y_true, y_pred = y_pred)))
}

#' Root mean square error (MSE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns root mean of squared errors
#'
#' @export
rmse <- function(y_true, y_pred, ...) {
  return(sqrt(mean(se(y_true = y_true, y_pred = y_pred))))
}

#' Geomatric mean absolute error (GMAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns geomatric mean of squared errors
#'
#' @export
gmae <- function(y_true, y_pred, ...) {
  return(g_mean(ae(y_true = y_true, y_pred = y_pred)))
}

# Percentage error measures --------------------------------------------------------------

#' Absolute percentage error (APE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns vector of absolute percentage errors
#'
#' @export
ape <- function(y_true, y_pred, ...) {
  return(100 * (ae(y_true = y_true, y_pred = y_pred) / abs(y_true)))
}

#' Symmetric absolute percentage error (sAPE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns vector of symmetric absolute percentage differences
#'
#' @export
sape <- function(y_true, y_pred, ...) {
  return(200 * (ae(y_true = y_true, y_pred = y_pred) / abs(y_true + y_pred)))
}

#' Mean absolute percentage error (MAPE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns mean of absolute percentage differences
#'
#' @export
mape <- function(y_true, y_pred, ...) {
  return(mean(ape(y_true = y_true, y_pred = y_pred)))
}

#' Symmetric mean absolute percentage error (sMAPE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#'
#' @return Returns mean of symmetric absolute percentage differences
#'
#' @export
smape <- function(y_true, y_pred, ...) {
  return(mean(sape(y_true = y_true, y_pred = y_pred)))
}

# Relative error measures --------------------------------------------------------------

#' Relative absolut error (RAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param y_bench forecasts of benchmark method
#'
#' @return Returns a vector of relative absolute differences
#'
#' @export
rae <- function(y_true, y_pred, y_bench, epsilon = 0.001, ...) {
  return(ae(y_true = y_true, y_pred = y_pred) / (ae(y_true = y_true, y_pred = y_bench) + epsilon))
}

#' Mean relative absolut error (MRAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param y_bench forecasts of benchmark method
#'
#' @return Returns mean of relative absolute differences
#'
#' @export
mrae <- function(y_true, y_pred, y_bench, ...) {
  return(mean(rae(
    y_true = y_true,
    y_pred = y_pred,
    y_bench = y_bench
  )))
}

#' Median relative absolut error (MRAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param y_bench forecasts of benchmark method
#'
#' @return Returns median of relative absolute differences
#'
#' @export
mdrae <- function(y_true, y_pred, y_bench, ...) {
  return(median(rae(
    y_true = y_true,
    y_pred = y_pred,
    y_bench = y_bench
  )))
}

#' Geomatric mean relative absolut error (GMRAE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param y_bench forecasts of benchmark method
#'
#' @return Returns geometric mean of relative absolute differences
#'
#' @export
gmrae <- function(y_true, y_pred, y_bench, ...) {
  return(g_mean(rae(
    y_true = y_true,
    y_pred = y_pred,
    y_bench = y_bench
  )))
}

# Scaled error measures --------------------------------------------------------------

#' Absolut scaled error (ASE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#'
#' @return Returns the absolute differences scaled by the mean in-sample error of naive
#'
#' @export
ase <- function(y_true, y_pred, in_sample, ...) {
  return(ae(y_true = y_true, y_pred = y_pred) / mean(abs(base::diff(in_sample, lag = 1))))
}

#' Seasonal absolut scaled error (ASE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#' @param frequency frequency of time series
#'
#' @return Returns the absolute differences scaled by the mean in-sample error of seasonal naive
#'
#' @export
sase <- function(y_true, y_pred, in_sample, frequency, ...) {
  return(ae(y_true = y_true, y_pred = y_pred) / mean(abs(base::diff(in_sample, lag = frequency))))
}

#' Mean absolut scaled error (MASE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#'
#' @return Returns mean of the absolute differences scaled by the mean in-sample error of naive
#'
#' @export
mase <- function(y_true, y_pred, in_sample, ...) {
  return(mean(ase(
    y_true = y_true,
    y_pred = y_pred,
    in_sample = in_sample
  )))
}

#' Seasonal Mean absolut scaled error (sMASE)
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#' @param frequency frequency of time series
#'
#' @return Returns the mean of the absolute differences scaled by the mean in-sample error of seasonal naive
#'
#' @export
smase <- function(y_true, y_pred, in_sample, frequency, ...) {
  return(mean(
    sase(
      y_true = y_true,
      y_pred = y_pred,
      in_sample = in_sample,
      frequency = frequency
    )
  ))
}

#' Mean Scaled interval score (MSIS)
#'
#' @param y_true true values of forecasts
#' @param upper upper bound
#' @param lower lower bound
#' @param alpha significance level
#' @param in_sample in_sample data
#' @param frequency frequency of data
#' @param ... additional parameters, not used
#'
#' @return Average MSIS
#' @export
msis <- function(y_true, upper, lower, alpha, in_sample, frequency, ...) {
  nom <- mean(
    (upper - lower) + # interval wideness
    ((2 / alpha) * (lower - y_true) * (y_true < lower)) + # outside of lower bound
    ((2 / alpha) * (y_true - upper) * (upper < y_true))   # outside of upper bound
  )
  d_nom <- mean(abs(base::diff(in_sample, lag = frequency)))
  return(nom / d_nom)
}

# summary functions ------------------------------------------------
#' Summary and detail of accuracy measures
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#' @param frequency frequency of time series
#'
#' @return list with summary and details of accuracy measures
#'
#' @export
accuracy <-
  function(y_true,
           y_pred,
           y_bench,
           upper,
           lower,
           alpha,
           in_sample,
           frequency,
           error_detail_functions = c('ae', 'se', 'ape', 'sape', 'rae', 'ase', 'sase'),
           error_summary_functions = c('mae',
                                       'mse',
                                       'gmae',
                                       'mape',
                                       'smape',
                                       'mrae',
                                       'mdrae',
                                       'gmrae',
                                       'mase',
                                       'smase',
                                       'msis')) {
  
    # convert to lower
    error_detail_functions <- tolower(error_detail_functions)
    error_summary_functions <- tolower(error_summary_functions)

    error_detail <-
      accuracy_detail(
        y_true = y_true,
        y_pred = y_pred,
        y_bench = y_bench,
        in_sample = in_sample,
        frequency = frequency,
        error_detail_functions = error_detail_functions
      )

    error_summary <-
      accuracy_summary(
        y_true = y_true,
        y_pred = y_pred,
        y_bench = y_bench,
        upper = upper,
        lower = lower,
        alpha = alpha,
        in_sample = in_sample,
        frequency = frequency,
        error_summary_functions = error_summary_functions
      )

    return(list(summary = error_summary,
                detail = error_detail))
  }

#' Summary of accuracy measures
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#' @param frequency frequency of time series
#'
#' @return summary and details of measures
#'
#' @export
accuracy_summary <-
  function(y_true,
           y_pred,
           y_bench,
           in_sample,
           upper,
           lower,
           alpha,
           frequency,
           error_summary_functions = c('mae',
                                       'mse',
                                       'gmae',
                                       'mape',
                                       'smape',
                                       'mrae',
                                       'mdrae',
                                       'gmrae',
                                       'mase',
                                       'smase',
                                       'msis'),
           ...) {
    # convert to lower
    error_summary_functions <- tolower(error_summary_functions)


    error_summary = sapply(seq.int(length.out = length(error_summary_functions)), function(i) {
      match.fun(error_summary_functions[i])(
        y_true = y_true,
        y_pred = y_pred,
        y_bench = y_bench,
        upper = upper,
        lower = lower,
        alpha = alpha,
        in_sample = in_sample,
        frequency = frequency
      )
    })

    names(error_summary) = error_summary_functions

    return(error_summary)
  }

#' Details (horizon specific) accuracy measures
#'
#' @param y_true actual value of time series
#' @param y_pred predicted value of time series
#' @param in_sample in sample (hystorical values) of time series
#' @param frequency frequency of time series
#'
#' @return horizon specific accuracy measures
#'
#' @export
accuracy_detail <-
  function(y_true,
           y_pred,
           y_bench,
           in_sample,
           frequency,
           error_detail_functions = c('ae', 'se', 'ape', 'sape', 'rae', 'ase', 'sase')) {
    # convert to lower
    error_detail_functions <- tolower(error_detail_functions)

    error_detail = sapply(seq.int(length.out = length(error_detail_functions)), function(i) {
      match.fun(error_detail_functions[i])(
        y_true = y_true,
        y_pred = y_pred,
        y_bench = y_bench,
        in_sample = in_sample,
        frequency = frequency
      )
    })

    colnames(error_detail) = error_detail_functions

    return(error_detail)
  }

#' Geometric Mean
#'
#' @param x Vector to take geometric mean from
#' @param na.rm Boolean (Optional, default: True) Wheater to remove NA values.
#'
#' @return geometric mean of vector
#' @export
g_mean <- function(x, na.rm = TRUE) {
  exp(sum(base::log(x[x > 0]), na.rm = na.rm) / length(x))
}
