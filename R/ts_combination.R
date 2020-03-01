#' Validation Split
#'
#' This function splits a time series into validation sets with training and test data; the output can be used to backtest models and/or select/combine models based on how they perfrom on this sets.
#'
#' @param y Time series onject.
#' @param val_h Horizon to be used for validation.
#' @param sov_only Flag, whether only single-origin validation should be considered. Optional, default \code{TRUE}.
#' @param val_min_years Minimum years required to conduct single origin validation. Optional, default \code{4}.
#' @param cv_min_years Minimum years required to conduct cross-origin validation. Optional, default \code{5}.
#' @param cv_max_samples Maximum samples that should be considered during cross-validation, i.e. 3 indicates the algorithm validates from 3 origins. Optional, default \code{3}.
#' @param ... not used
#'
#' @return list of validation sets
#' @export
validation_split <- function(y,
                             val_h,
                             sov_only = F,
                             val_min_years = 4,
                             cv_min_years = 5,
                             cv_max_samples = 3,
                             ...) {
    # split for the last possible validation set
    sov_split <- (length(y) - val_h)
    # crossvalidation list placeholder
    cv_set <- NA
    # determine validation technique to perform single of cross validation
    if (sov_split < (frequency(y) * val_min_years)) {
      # no validation possible due to little data
      return(NA)
    } else if (sov_split < (frequency(y) * cv_min_years) || sov_only) {
      # single origin validation
      cv_set <- list(
        list(
          train = head(y, n = sov_split),
          test = tail(y, n = val_h)
        )
      )
    } else {
      # cross validation
      sample_space <- (frequency(y) * cv_min_years):sov_split
      # reverse order because last year has to be part of the validation
      # find better a way if there are performance issues
      splits <- tail(rev(round(
        seq(from = max(sample_space),
            to = min(sample_space),
            by = -((max(sample_space) - min(sample_space))/ cv_max_samples)
      ))), n = cv_max_samples)
      # specify place holder for cross validation
      cv_set <- vector(mode = 'list', length = length(splits))
      # generate cross validation sets
      for (i in seq.int(length.out = length(splits))) {
        cv_set[[i]] <-
          list(
            train = head(x = y, n = splits[i]),
            test = y[(splits[i] + 1):(splits[i] + val_h)]
          )
      }
    }
    return(cv_set)
}


#' Forunco combination approach
#'
#' Combination of univariate time seris methods, using a combination operator of choice for the mean forecasts and prediction intervals.
#'
#' @param y Time series object with historical values.
#' @param h Horizons to be predicted.
#' @param levels Prediction interval levels. Optional, default \code{c(95)}.
#' @param methods Methods to be combined. Optional, default \code{auto_ets, auto_arima, auto_thetaf}.
#' @param point_combination Point forecast combination operator. Optional, default \code{meidan}.
#' @param pi_combination_upper combination operator of the upper bound of the prediction intervals. Optional, default \code{max}.
#' @param pi_combination_lower combination operator for the lower bounds of the prediction intervals. Optional, default \code{min}.
#' @param pool_limit number of methods selected from the pool to reach the final forecasts. Optional, default \code{length(methods)}.
#' @param error_fun error function to determine the validation performance Optional, default \code{rmse}.
#' @param weight_fun weight function for calculating the definitive weights for combination. Optional, default \code{inverse}
#' @param val_h The horizon for the validation samples. Optional, default \code{h}.
#' @param sov_only Flag indicating whether only single origin validation should be considered. Optional, default \code{TRUE}.
#' @param max_years Maxmium of years to consider during model fitting. Optional, default \code{30}.
#' @param val_min_years Minimum years required to conduct single origin validation. Optional, default \code{4}.
#' @param cv_min_years Minimum years required to conduct cross-origin validation. Optional, default \code{5}.
#' @param cv_max_samples Maximum samples that should be considered during cross-validation, i.e. 3 indicates the algorithm validates from 3 origins. Optional, default \code{3}.
#' @param allow_negatives Flag indicating whether to allow negative values or not. Optional, default \code{FALSE}.
#' @param remove_outliers Flag indicating whether to automatically remove outliers form the time series. default \code{FALSE}
#' @param ... passed to the forecasting functions
#'
#' @return combined forecasts of time series y including confidence intervals
#' @examples
#' # not run
#' library(Mcomp)
#' ts <- M3[[2104]]$x
#' fcs <- forunco(y = ts, h = 18)
#' # end not run
#' @export
forunco <- function(y,
                    h,
                    levels = c(95),
                    methods = c('auto_ets', 'auto_arima', 'auto_thetaf'),
                    pp_methods = c('boxcox'), 
                    point_combination = 'median',
                    pi_combination_upper = 'median',
                    pi_combination_lower = 'median',
                    pool_limit = length(methods),
                    error_fun = 'rmse',
                    weight_fun = 'inverse',
                    val_h = h,
                    sov_only = F,
                    max_years = 30,
                    val_min_years = 4,
                    cv_min_years = 5,
                    cv_max_samples = 3,
                    allow_negatives = F,
                    remove_outliers = F,
                    ...) {
  require(dplyr)
  # provide a check that ensure the methods and combination methods are valid
  if (class(y) != 'ts')
    stop("Time series object required.")
  
  fqy <- frequency(y)
  
  # replacement of outliers
  if (remove_outliers)
    y <- replace_outliers(y = y, frequency = fqy, ...)

 # preprocessing
  unique_pp_methods <- unique(pp_methods)
  
  # preprocessed time series
  if (is.null(pp_methods) | any(!(pp_methods %in% c('scale', 'normalize', 'diff', 'log', 'boxcox', 'seasonal_adjustment'))))
    pp_methods <- c('no_pp')
  
  prop <- preprocessor()
  pp_y <- prop(y = y, frequency = fqy, action = 'transform', operations = unique_pp_methods, h = h, ...)

  # for readibility, initialize final variables
  point <- upper <- lower <- NA
  
  # combination type
  wc <- F

  # define simple combinations that do not require validation weigth calculation
  sc <- c('mean', 'median', 'min', 'max')

  # if too litle data is available return to the defaults
  if ((length(pp_y) - val_h) < (frequency(pp_y) * val_min_years)) {
    point_combination <- 'median'
    pi_combination_upper <- 'median'
    pi_combination_lower <- 'median'
  }

  # if a simple combination is selected, simply produce the forecasts and combine them
  # note the if such a method is used, then validation method (sov_only) can be disregarded, as
  # these methods do not depend on validation errors.
  if (point_combination %in% sc &
      pi_combination_upper %in% sc &
      pi_combination_lower %in% sc &
      pool_limit >= length(methods)) {
    # perform a simple combination with no selection
    fcs <- simple_combination(y = pp_y,
                              h = h,
                              levels = levels,
                              methods = methods,
                              point_combination = point_combination,
                              pi_combination_upper = pi_combination_upper,
                              pi_combination_lower = pi_combination_lower)
  } else {
    # split time series into validation sets; based on the available data or user specifications in parameter sov_only
    validation_set <- validation_split(y = pp_y,
                                       val_h = val_h,
                                       sov_only = sov_only,
                                       max_years = max_years,
                                       val_min_years = val_min_years,
                                       cv_min_years = cv_min_years,
                                       cv_max_samples = cv_max_samples)

    # check if validation is available, only if validation_split and forunco are no longer synchronised
    if (identical(validation_set, NA)) {
      warning('No validation set could be established with the selected parametrization; thus simple combination with default parametrization is selected...
              Note that this method does not select methods from a pool.')
      # produce forecasts
      fcs <- simple_combination(y = pp_y,
                                h = h,
                                levels = levels,
                                methods = methods,
                                point_combination = 'median',
                                pi_combination_upper = 'median',
                                pi_combination_lower = 'median')

    } else {
      # ser weighted combination to true
      wc <- T
      # 3d array for the point forecasts of all validation sets
      val_point_array <- array(NA, dim = c(val_h, length(methods), length(validation_set)))
      # get forecasts and predeiction bounds
      predictions <- produce_forecasts(y = pp_y, h = h, levels = levels, methods = methods, ...)
  
      # produce forecasts for all validation sets
      for (i in seq.int(length.out = length(validation_set))) {
        val_point_array[, , i] <- produce_forecasts(y = validation_set[[i]]$train, h = val_h, levels = levels, methods = methods, ...)$point
      }
  
      # if user specifies mean or median and want to pool data, then adjust the paramter setting
      if (point_combination %in% c('mean', 'median')) {
        # set weight function to the point combination, set prefix
        weight_fun <- paste('pool_', point_combination, sep = '')
        # and update point_combination
        point_combination <- 'weighted_average'
      }
      # weight forecast with validation error of validation set,
      # do this for all the validation sets and return the 
      # resulting forecast 
      candidates <- lapply(seq.int(length.out = length(validation_set)), function(i) {
        match.fun(point_combination)(val_hat = val_point_array[, , i],
                                     val_true = validation_set[[i]]$test,
                                     y_hat = predictions$point,
                                     error_fun = error_fun,
                                     weight_fun = weight_fun,
                                     pool_limit = pool_limit,
                                     ...)
      })
      
      # set the point forecast to the weighted forecasts of the different validation set
      # and set the combination to mean; so that i.e. the forecasts ist the mean of the
      # weighted forecasts calculated using the validation sets above.
      
      # idea: decaying importance of validation error as the validation set uses older 
      # data. Potentially, this would be easily implemented with another cost function in 
      # ts_operator.R 
      predictions$point <- sapply(candidates, function(x) x$y_hat)
      fcs <- simple_combination(y = pp_y,
                                h = h,
                                fcs_mats = predictions,
                                levels = levels,
                                methods = methods,
                                point_combination = 'mean',
                                pi_combination_upper = pi_combination_upper,
                                pi_combination_lower = pi_combination_lower)
    }
  }
  # backtransformation
  #point
  point <- prop(y = fcs$point, action = 'backtransform', is_fcs = T, frequency = fqy, ...)
  # upper
  upper <- apply(fcs$upper, 2, function(x) {
    prop(y = fcs$upper, action = 'backtransform', is_fcs = T, frequency = fqy, ...)
  })
  # lower
  lower <- apply(fcs$lower, 2, function(x) {
    prop(y = fcs$lower, action = 'backtransform', is_fcs = T, frequency = fqy, ...)
  })
  
  # bind result sets
  date <- time_sequence(y = y, out_length = (h + 1))[-1]
  pred_mat <- cbind(point, upper, lower)
  colnames(pred_mat) <- c('Point', paste('U', levels, sep = ''), paste('L', levels, sep = ''))
  
  # remove zeros
  if(!allow_negatives)
    pred_mat[pred_mat < 0] <- 0
  
  # prepare result data_frames
  res <- pred_mat %>%
    dplyr::as_data_frame() %>%
    tibble::add_column(date, .before = 1) %>%
    tidyr::gather(type, value, 2:(NCOL(pred_mat) + 1)) %>% 
    dplyr::mutate(value = as.double(value))
  
  # parametrization
  params <- list(
    fcs = 
      list(
        horizons = h,
        methods = methods,
        pi_levels = levels,
        max_years = max_years,
        allow_negatives = allow_negatives
    ),
    comb = list(
      pp_methods = pp_methods, 
      point_combination = point_combination,
      pi_combination_upper = pi_combination_upper,
      pi_combination_lower = pi_combination_lower
    )
  )

  if (wc) {
    params$cv = list(
      pool_limit = pool_limit,
      error_fun = error_fun,
      weight_fun = weight_fun,
      val_h = val_h,
      val_min_years = val_min_years,
      cv_min_years = cv_min_years,
      cv_max_samples = cv_max_samples,
      results = candidates
    )
  } 
  
  # workaround, find better solution
  s_space <- 1:(2 * length(levels) + 1)
  
  
  # return res
  return(
    structure(
      list(
        mean = pred_mat[, 1],
        upper = pred_mat[, head(s_space, length(levels)) + 1],
        lower = pred_mat[, tail(s_space, length(levels)) ],
        preds = res,
        actuals = tidier_ts(y),
        params = params
      )
      , class = 'forunco')
  )
}

#' Simple combination of univariate time series forecasting methods
#'
#' Combines muultiple forecasts using definable combination operators.
#'
#' @param y Time series object.
#' @param h Horizons to be predicted.
#' @param levels Prediction interval levels. Optional, default \code{c(95)}.
#' @param methods Methods to be combined. Optional, default \code{auto_ets, auto_arima, auto_thetaf}.
#' @param point_combination Point forecast combination operator. Optional, default \code{meidan}.
#' @param pi_combination_upper combination operator of the upper bound of the prediction intervals. Optional, default \code{max}.
#' @param pi_combination_lower combination operator for the lower bounds of the prediction intervals. Optional, default \code{min}.
#' @param allow_negatives Flag indicating whether to allow negative values or not. Optional, default \code{FALSE}.
#' @param ... passed to the forecasting functions
#'
#' @return combined forecasts of time series y including confidence intervals
#' @export
simple_combination <- function(y, 
                               h, 
                               fcs_mats = NA, 
                               levels, 
                               methods, 
                               point_combination = 'median',
                               pi_combination_upper = 'median', 
                               pi_combination_lower = 'median', 
                               allow_negatives = F, 
                               ...) {
  #
  if(identical(fcs_mats, NA)) {
    if(identical(y, NA))
      stop('Either forecasting matrices or the historical values must be provided.')

    if(identical(h, NA))
      stop('Horizon has to be provided for calculating the forecasts...')

    fcs_mats <- produce_forecasts(y = y, h = h, levels = levels, methods = methods, ...)
  }
  
  # upper and lower placeholder
  upper <- lower <- matrix(data = NA, nrow = h, ncol = length(levels))
  
  # combine point forecasts
  point <- generic_combine(fcs_mats$point, c_fun = point_combination)
  
  # catch error for weighted average
  if (dim(fcs_mats$point)[2] == 1) {
    # if only one point forecast
    point_mat <- matrix(data = rep(fcs_mats$point, dim(fcs_mats$upper)[2]), ncol = 9, nrow = 18)
  } else {
    point_mat <- fcs_mats$point
  }
  
  # combine upper bounds
  upper_input <- sweep(fcs_mats$upper, 1:2, point_mat, '/')
  upper <- apply(upper_input, 3, function(x) {
    generic_combine(x, c_fun = pi_combination_upper)
  })
  upper <- sweep(upper, MARGIN = 1, STATS = point, '*')
  
  # combine lower bounds
  lower_input <- sweep(fcs_mats$lower, 1:2, point_mat, '/')
  lower <- apply(lower_input, 3, function(x) {
    generic_combine(x, c_fun = pi_combination_lower)
  })
  lower <- sweep(lower, MARGIN = 1, STATS = point, '*')

 return(
   list(
     point = point,
     upper= upper,
     lower = lower
   )
 )
}

#' Produces forecasts
#'
#' Using the predefined methods, this function produces point forecasts as well as prediction intervals of any given level.
#'
#' @param y Time series object.
#' @param h Horizons to be predicted.
#' @param levels Prediction interval levels. Optional, default \code{c(95)}.
#' @param methods Methods to be combined. Optional, default \code{auto_ets, auto_arima, auto_thetaf}.
#' @param pp_obj preprocessing parameters
#' @param ... passed to the forecasting functions
#'
#' @return combined forecasts of time series y including confidence intervals
#' @export
produce_forecasts <- function(y,
                              h,
                              levels = c(80, 95),
                              methods = c('auto_ets', 'auto_arima', 'auto_thetaf'),
                              ...) {

  # a method can only be defined once
  unique_methods <- unique(methods)
 
  # construct a matrix that holds the prediction for the combination later on
  fcs_objs <- lapply(unique_methods, function(m) {
    match.fun(m)(y = y, h = h, level = levels)
  })

  # combine PIs into matrix for easy combination
  point_mat <- array(NA, dim = c(h, length(unique_methods)))
  upper_mat <- lower_mat <- array(NA, dim = c(h, length(unique_methods), length(levels)))

  # loop over methods and store pis in 3d matrix
  for (i in seq.int(length.out = length(unique_methods))) {
    point_mat[, i] <-   fcs_objs[[i]]$mean
    upper_mat[, i, ] <- fcs_objs[[i]]$upper
    lower_mat[, i, ] <- fcs_objs[[i]]$lower
  }

  # return result
  return(list(
    point = point_mat,
    upper = upper_mat,
    lower = lower_mat
  ))
}
