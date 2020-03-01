#' Generic combination function
#'
#' This function is used for simpler combination methods supported out of the box from the stats package in R; such as e.g. \code{mean} or \code{median}
#'
#' @param y_hat predicted values for test set
#' @param c.fun combination funciton
#' @param ...
#'
#' @return combined forecasts
#' @export
generic_combine <- function(y_hat, c_fun, ...) {
  res <- apply(y_hat, 1, function(x) {
    match.fun(c_fun)(x, ...)
  })
  return(res)
}


#' Averaging forecasts using weights
#'
#' Combines forecasting methods based on the validation error. See Nowotarski, J., Raviv, E., Tr\"uck, S., and Weron, R. (2014) for more examples.
#'
#' @param val_hat forcasts on the validation set
#' @param val_true true values of the validation set
#' @param y_hat forecasts for the test set
#' @param error_measure error measure to be used for error calculation; error measures that calculate errors per horizons will lead to a horizon specific combination, error measures over all horizons on the other hand, will lead to a simple weighted combination.
#' @param weight_fun the function to determine the weights
#' @param pool_limit how many methods should be considered for combination
#' @param ... not used
#'
#' @return combined forecasts using imrse for weighting
#'
#' @references
#'
#' Nowotarski, J., Raviv, E., Tr\"uck, S., and Weron, R. (2014). An Empirical Comparison of Alternative
#' Schemes for Combining Electricity Spot Price Forecasts. \emph{Energy Economics}, \bold{46}, 395--412.
#' @export
weighted_average <- function(val_hat,
                             val_true,
                             y_hat,
                             error_fun = 'rmse',
                             weight_fun = 'inverse',
                             pool_limit = 3,
                             ...) {
  # calculate error measure
  error <- apply(val_hat, 2, function(v_h) {
    match.fun(error_fun)(y_true = val_true, y_pred = v_h, ...)
  })

  # check output, meaning what error measure has been selected
  if (is.null(dim(error))) {
    # if median is defined as weight fun, throw an error as not
    # applicable for global weightening
    if(weight_fun == 'pool_median')
      stop('Median cannot be defined with an averaged error measure, please select an
           error measure that calculates the error measure per individual horizon...')
    # meaning that global weightening has been selected
    # one weight for one method (vector); note that the pool_limit
    # determines how many methods can be selected
    weights <- weight_error(weight_fun = weight_fun, error = error, pool_limit = pool_limit)
    # weight forecasts and return y_hat
    y_hat <- apply(sweep(y_hat, 2, weights, FUN = '*'), 1, sum)
  } else if (identical(dim(y_hat), dim(error))) {
    # weights can be calculated per horizon, meaning that one error per
    # horizon is available and, thus, methods can be selected on a
    # horizon basis -> e.g. for horizon 1 (arima, ets), horizon 2 (theta, naive)...
    weights <- t(sapply(1:NROW(error), function(i) {
      weight_error(weight_fun = weight_fun, error = error[i, ], y_hat = y_hat[i, ], pool_limit = pool_limit)
    }))
    # weight forecasts on a horizon level
    y_hat <- apply((y_hat * weights), 1, sum)
  } else {
    # error because different values for val_h and h are selected
    stop('Error in weighting combination forecasts, most probable reason is the selection of horizon
         specific combination and different values for h and val_h... If horizon specific validation
         is desired, please set these parameters to the same value...')
  }

  # return value
  return(
    list(
      y_hat = y_hat,
      accuracy = error,
      weights = weights
    )
  )
}


#' Weighting and pooling methods on the basis of their error
#'
#' @param weight_fun weight function
#' @param error the error vector which should be weightened
#' @param pool_limit the pool size to be considered (e.g. 3 would indicate that only the best three methods would be selected)
#' @param ... passed to weight_fun
#'
#' @return the weights
#' @export
weight_error <- function(weight_fun, error, y_hat, pool_limit, ...) {
  # set up result set,
  # note: methods that are not selected have a weight of 0
  res <- rep(0, length(error))
  # rank the methods according to their error
  method_ranks <- rank(error, ties.method = 'min')
  # determine the weights and update the res vector
  res[which(method_ranks <= pool_limit)] <-
    match.fun(weight_fun)(x = error[which(method_ranks <= pool_limit)], fcs = y_hat[which(method_ranks <= pool_limit)], ...)
  return(res)
}

#' Calculates the mean of the pool
#'
#' @param fcs pooled forecasts
#' @param ... not used
#'
#' @return the weights
#' @export
pool_median <- function(fcs, ...) {
  # get the median forecast
  median_fcs <- as.numeric(which(fcs == median(fcs)))
  # init 0 array
  res <- rep(0, length(fcs))
  res[median_fcs] <- 1
  return(res)
}

#' Calculates the mean of the pool
#'
#' @param x error metrics (higher is worse)
#' @param ... not used
#'
#' @return the weights
#' @export
pool_mean <- function(x, ...) {
  return(rep((1/length(x)), length(x)))
}

#' Calculates the inverse
#'
#' @param x error metrics (higher is worse)
#' @param ... not used
#'
#' @return the weights
#' @export
inverse <- function(x, ...) {
  inv_error <- x ^ -1
  return(inv_error / sum(inv_error))
}

#' Calculates the squared inverse
#'
#' @param x error metrics (higher is worse)
#' @param ... not used
#'
#' @return the weights
#' @export
squared_inverse <- function(x, ...) {
  inv_error <- (x ^ 2) ^ -1
  return(inv_error / sum(inv_error))
}

#' Calculates the exponential inverse
#'
#' @param x error metrics (higher is worse)
#' @param ... not used
#'
#' @return the weights
#' @export
exp_inverse <- function(x, ...) {
  inv_error <- exp(x) ^ -1
  return(inv_error / sum(inv_error))
}
#
# end file