#' Scale vector
#'
#' @param y vector of occurences
#' @param ...
#'
#' @return list with two members (res and param); where res is the
#' preporcessed vector and param the used parameters
#' @export
scale <- function(y, ...) {
  param <- list(mean = mean(y), sd = sd(y))
  res <- (y - param$mean) / param$sd
  return(list(res = res,
              param = param))
}

#' Inverses scaling
#'
#' Reverses the scaling conducted in function \code{scale()}.
#'
#' @param y vector to be rescaled
#' @param param parameters used for the original scaling
#' @param ...
#'
#' @return rescaled vector
#' @export
inv_scale <- function(y, param = NA, ...) {
  if (identical(param, NA)) {
    stop("No scaling parameter available.")
  }
  return((y * param$sd) + param$mean)
}

#' Normalize vector
#'
#' @param y vector of occurences
#' @param ...
#'
#' @return list with two members (res and param); where res is the
#' preporcessed vector and param the used parameters
#' @export
normalize <- function(y, ...) {
  param <- list(min = min(y), may = may(y))
  res <- (y - param$min) / (param$may - param$min)
  return(list(
    res = res,
    param = param
  ))
}

#' Inverses normalization
#'
#' Reverses the normalization conducted in function \code{normalize()}.
#'
#' @param y vector to be rescaled
#' @param param parameters used for the original scaling
#' @param ...
#'
#' @return renormalized vector
#' @export
inv_normalize <- function(y, param = NA, ...) {
  if (identical(param, NA)) {
    stop("No scaling parameter available.")
  }
  return(y * (param$may - param$min) + param$min)
}

#' Differencing of a vector
#'
#' Calculates the differences of time series over time.
#'
#' @param y vector which is correctly ordered with regards to time
#' @param n.diffs integer (default: 1) of differences to be conducted
#' @param auto boolean (default: T), whether number of differences should be defined automatically
#' @param ...
#'
#' @return list with two members (res and param); where res is the
#' preporcessed vector and param the used parameters
diff <- function(y, n.diffs = 1, auto = T, ...) {

  # support for automatically determining number of differencing
  n_diffs <- ifelse(auto, ndiffs(x = y), n.diffs)
  # list of parameters
  param <- list(
    xi_y = y[1],
    y_ori = y,
    xi_fcs = y[length(y)],
    n_diffs = n_diffs
  )
  # default result set
  res <- y
  # check if differencing is even required
  if (n_diffs > 0 | !missing(n_diffs)) {
    # mutliple differences have to tracked to properly
    # inverse the differences
    if (n_diffs > 1) {
      pp_y <- y
      xi_y <- xi_fcs <- c()
      # store values for inversing diff later on
      for (i in 1:n_diffs) {
        xi_fcs <- c(pp_y[length(pp_y)], xi_fcs)
        xi_y <- c(pp_y[1], xi_y)
        pp_y <- base::diff(x = pp_y, differences = 1)
      }
      param$xi_y <- xi_y
      param$xi_fcs <- xi_fcs
      res <- pp_y
    } else {
      res <- base::diff(x = y, differences = 1)
    }
  }
  # return the result
  return(list(
    param = param,
    res = res
  ))
}


#' Inverses diff transformations
#'
#' Backtransformes diff transformations performed in \code{diff()}. Support for multiple transformations.
#'
#' @param y time series vector
#' @param param parameter of the transformation function
#' @param is_fcs boolean, default (T). whether it is a forecast or in-sample vector.
#' @param ... not used
#'
#' @return backtransformed time series
#' @export
inv_diff <- function(y, param, is_fcs = T, ...) {
  # store transformation when multiple
  pp_y <- y
  for (i in 1:param$n_diffs) {
    pp_y <- stats::diffinv(x = pp_y, differences = 1, xi = ifelse(is_fcs, param$xi_fcs[i], param$xi_y[i]))
  }

  return(pp_y[-(1:param$n_diffs)])

}

#' Log tranformation
#'
#' @param y time series vecotr
#' @param ... not used
#'
#' @return transformed time series
#' @export
log <- function(y, ...) {
  y[y <= 0] <- 0.01 # do not allow zeros
  return(base::log(y))
}

#' Inverse of log transformation
#'
#' Used to automatically perfrom multiply transformations using the preprocessor environment; basicall calculates \code{exp} of input \code{y}.
#'
#' @param y time series vector.
#' @param ... not used
#'
#' @return backtransformed time series
#' @export
inv_log <- function(y, ...) {
  y[y <= 0] <- 0.01 # do not allow zeros
  return(exp(y))
}

#' BoxCox transformation
#'
#' BoxCox transformation wrapper around the function \code{forecast::BoxCox()}.
#' @param y time series vector
#' @param ... not used
#'
#' @return list with two members; res is the transformed time series and param the used parameters for the transformation.
#' @export
boxcox <- function(y, ...) {
  param <- forecast::BoxCox.lambda(y, lower = 0)
  y[y <= 0] <- 0.01 # do not allow zeros
  res <- forecast::BoxCox(x = y, lambda = param)
  return(list(
    param = param,
    res = res
  ))
}

#' Inverse Boxcox transformation
#'
#' Inverses the BoxCox transformation conducted in function \code{boxcox}. The funciton is a wrapper around the function \code{forecast::InvBoxCox()}.
#'
#' @param y time series
#' @param param parameter list
#' @param ... not used
#'
#' @return backtransformed time series vector
#' @export
inv_boxcox <- function(y, param, ...) {
  y[y < 0] <- 0.01 # do not allow zeros
  return(forecast::InvBoxCox(x = y, lambda = param))
}

#' Conductes a seasonal adjustment
#'
#' Seasonally adjusts a time series vector using the multiplicative decomposition approach.
#'
#' @param y time series vector
#' @param frequency frequency of time series
#' @param h horizon of time series
#' @param ... not used
#'
#' @return list with two members; res is the seasonally adjusted time series; param the seasonal components to reconstruct the time series or forecasts in \code{inv_seasonal_adjustment}.
#' @export
seasonal_adjustment <- function(y, frequency = frequency, h = h, type = 'M', ...) {

  if(identical(NA, frequency))
    stop('Frequency is needed to seasonal adjust the time series.')

  decomp_type <- switch(type,
                        'M' = 'multiplicative',
                        'A' = 'additive')

  if(is_seasonal(y = y, frequency = frequency) & type %in% c('M', 'A')) {
    components <- decompose(y ,type = decomp_type)
    s_in <- components$seasonal
    # deseasonalize data
    if (type == 'M') {
      y <- y / s_in
    } else {
      y <- y - s_in
    }
    # repeat the seasonal component to cover horizons longer than seasonality
    s_out <- head(rep(components$seasonal[(length(components$seasonal) - frequency + 1):length(components$seasonal)], h), h)
  } else {
    s_in <- rep(1, length(y))
    s_out <- rep(1, h)
  }
  return(list(
    param = list (
      type = type,
      s_out = s_out,
      s_in = s_in
      ),
    res = y
  ))
}

#' Inversed seasonal adjustment
#'
#' Backtransforms the seasonal adjustment conducted in function \code{seasonal_adjustment}.
#'
#' @param y time series
#' @param param parameter list
#' @param ... not used
#'
#' @return Re-seasonalized time series
#'
#' @references
#'  Makridakis SG, Wheelwright SC, Hyndman RJ (1998). Forecasting: Methods and applications ( Third Edition). New York: Wiley.
#'
#' @export
inv_seasonal_adjustment <- function(y, param, ...) {
  
  if (length(y) == length(param$s_out)) {
    s_comp <- param$s_out
  } else if (length(y) == length(param$s_in)) {
    s_comp <- param$s_in
  } else {
    warning('Length of time series did not match the in- or out-of-sample data: assuming issues with differencing and in-sample backtransformation.
            Extracting last length(y) seasonal components for re-seasonalization. Please check if this applies for you!')
    s_comp <- tail(param$s_in, length(y))
  }
  # deseasonalize input
  if (param$type == 'N') {
    return(y + s_comp)
  } else {
    return(y * s_comp)
  }
}


#' Replacement of outliers
#'
#' This function is a wrapper around the funciton \code{forecast::tsoutliers()}.
#'
#' @param y time series vector
#' @param frequency frequency of time series vector
#' @param start start date, default (1)
#' @param ... not used
#'
#' @return timeseries without outliers
#' @export
replace_outliers <- function(y, frequency, start = 1, ...) {
  if (class(y) != 'ts')
    y <- ts(y, start = start, frequency = frequency)

  outliers <- forecast::tsoutliers(y)
  if (length(outliers) != 0) {
    for (i in seq.int(outliers$index)) {
      y[outliers$index[i]] <- outliers$replacements[i]
    }
  }
  return(y)
}


#' Seasonality test
#'
#' @param y time series
#' @param frequency frequency of time series vector
#' @param ... not used
#'
#' @return boolean (true if seasonal, false otherwise)
#'
#' @references
#'  Makridakis SG, Wheelwright SC, Hyndman RJ (1998). Forecasting: Methods and applications ( Third Edition). New York: Wiley.
#' @export
is_seasonal <- function(y, frequency = frequency(y), ...) {
  tcrit <- 1.645
  if (length(y) < 3 * frequency | frequency == 1) {
    res <- FALSE
  } else {
    xacf <- acf(y, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit / sqrt(length(y)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    res <- (abs(xacf[frequency]) > clim[frequency])

    if (is.na(res) == TRUE) {
      res <- FALSE
    }
  }
  return(res)
}


#' Dummy PP 
#'
#' @param y time series
#' @param ... 
#'
#' @return same time series
#' @export
no_pp <- function(y, ...) {
  return(y)
}

#' Dummy inv PP 
#'
#' @param y time series
#' @param ... 
#'
#' @return same time series
#' @export
inv_no_pp <- function(y, ...) {
  return(y)
}

#' Preprocessor environment
#'
#' Can be used to flexibly transforming and backtransforming data. Generally all functions - including custom ones in the current environment are supported.
#' The function assumes that each transformation funcotion (e.g. \code{normalize <- function (x, ...) {}}) has a backtransformation function with the same name
#' but a \code{inv_} prefix (e.g. \code{inv_normalize <- function(x, param, ...){}}). All parameter used for transforming the variables can be stored in the
#' param variable; which will be passed automatically back to the backtransformation function.
#'
#' @return returns either transformed or backtransformed time series vector
#'
#' @details The preprocessor has to "instantiated" and can then be called using the following parameters:
#' \itemize{
#'  \item{"y"}{time series or forecast to be transformed/backtransformed}
#'  \item{"action"}{action to be taked, either 'transform' or 'backtransform'}
#'  \item{"operations"}{transformatoin operations to be takes (functions to be called)}
#'  \item{"n.diffs"}{integer default (NA). Sets the number of differencing to be taken (used only when diff is part of operator)}
#'  \item{"auto"}{boolean, default (T), whether the number of differencing is to be determined automatically}
#'  \item{"is_fcs"}{boolean ,default (T), wheter it is forecast or in-sample data to be transformed}
#'  \item{"frequency"}{inteher, default (NA), the frequency of the time series (used for seasonal adjustment)}
#'  \item{"h"}{horizon, default (1)}
#' }
#'
#' @export
#'
#' @examples
#' # not run
#' # load demo data
#' library(Mcomp)
#' # get demo time series
#' demo_ts <- M3[[1560]]$x
#' # show demo ts
#' demo_ts
#' # get preprocessor "object" (environment)
#' pp <- preprocessor()
#' # preprocess time series
#' y_pp <- pp(y = demo_ts,
#'            action = 'transform',
#'            operations = c('seasonal_adjustment', 'scale'),
#'            frequency = frequency(demo_ts))
#' # show preprocessed time series
#' y_pp
#' # ok, now go back to the roots
#' y_back <- pp(y = y_pp, action = 'backtransform', is_fcs = F)
#' y_back - demo_ts
#' # end not run
preprocessor <- function() {

  # hold/manage transformations and their corresponding parameters
  order <- NA
  params <- list()

  function(y, action = 'transform', operations = c('diff', 'scale'), n.diffs = NA, auto = T, is_fcs = T, frequency = frequency(y), h = 1, type = 'M', ...) {

    pp_y <- y
    # transform or inverse transform feautres
    if (action == 'transform') {
      # set order
      order <<- operations
      param <<- list()
      # transform features
      for (x in order) {
        # get result list from the respective function
        res_list <- match.fun(x)(y = pp_y, n.diffs = n.diffs, auto = auto, frequency = frequency, h = h, type = type, ...)
        # store preprocessed time series
        pp_y <- res_list$res
        # store the parameter in a generic form for back-transformation --> dictionary
        params[[x]] <<- res_list$param
      }
      return(pp_y)
    } else if (action == 'backtransform') {
      # apply the transformations in the reverse order
      # remember to add inv_ to get the backtransformation function
      for (x in rev(order)){
        pp_y <- match.fun(paste('inv_', x, sep = ''))(y = pp_y, param = params[[x]], is_fcs = is_fcs, frequency = frequency, h = h, ...)
      }
      return(pp_y)
    }
  }
}


# garbage
# nsdiff not working somehow
# }
# if (seasonal & (nsdiffs(ts) > 0 | !missing(n.diffs))) {
#  ts.ns.diff <- ifelse(n.diffs == 0, nsdiffs(ts), n.diffs)
#  ts.is.diffed <- T
#  return(base::diff(x = ts, lag = lag, differences = ts.n.diff))
# } else {
#  return(ts)
# }

