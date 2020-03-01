  
#' Auto Complex exponential Smoothing
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_ces <- function(y, h, level, ...) {
  require(smooth)
  return(forecast::forecast(smooth::auto.ces(data = y), h = h, level = (level/100)))
}

#' Auto Arima
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_arima <- function(y, h, level, ...) {
  return(forecast::forecast(forecast::auto.arima(y = y), h = h, level = level))
}

#' Auto ETS
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_ets <- function(y, h, level, ...) {
  return(forecast::forecast(forecast::ets(y = y), h = h, level = level))
}

#' Auto thetaf
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_thetaf <- function(y, h, level, ...) {
  return(forecast::thetaf(y = y, h = h, level = level, ...))
}


#' Auto Theta dotm
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_dotm <- function(y, h, level, ...) {
  return(forecTheta::dotm(y = y, h = h, level = level, ...))
}

#auto_nn <- function(y, h, level, ...) {
#  return(forecast::forecast(forecast::nnetar(y = y), h = h, level = level))
#}

#' Auto Damped Exponentional Smoothing
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_damped <- function(y, h, level, ...) {
  fcs <- generic_forecast(fcs.fun = 'holt', y = y, h = h, damped = T, level = level, ...)
  return(fcs)
}

#' Auto Holt Exponential Smoothing
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_holt <- function(y, h, level, ...) {
  fcs <- generic_forecast(fcs.fun = 'holt', y = y, h = h, damped = F, level = level, ...)
  return(fcs)
}

#' Auto Simple Exponential Smoothing
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_ses <- function(y, h, level, ...) {
  fcs <- generic_forecast(fcs.fun = 'ses', y = y, h = h, level = level, ...)
  return(fcs)
}

#' Auto Naive
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_naive <- function(y, h, level, ...) {
  fcs <- generic_forecast(fcs.fun = 'naive', y = y, h = h, level = level, ...)
  return(fcs)
}

#' Auto seasonal naive
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_snaive <- function(y, h, level, ...) {
  fcs <- forecast::snaive(y = y, h = h, level = level, ...)
  return(fcs)
}

#' Auto autoregressive neural networke
#' 
#' Experimental state.
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_nnar <- function(y, h, level, ...) {
  fcs <- forecast::forecast(forecast::nnetar(y = y, repeats = 30), h = h, level = level, PI = TRUE, ...)
  return(fcs)
}


#' Auto SHD Combo
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
auto_shd <- function(y, h, level, ...) {
  fcs <- generic_forecast(fcs.fun = 'shd', y = y, h = h, level = level, ...)
  return(fcs)
}

#' Conventional SHD Combination
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
shd <- function(y, h, level, ...) {

  sim <- forecast::ses(y = y, h = h, level = level)
  hol <- forecast::holt(y = y, h = h, level = level, damped = F)
  dam <- forecast::holt(y = y, h = h, level = level, damped = T)

  # nicer way would be great
  res <- list()
  res$mean <- (sim$mean + hol$mean + dam$mean) / 3
  res$upper <- (sim$upper + hol$upper + dam$upper) / 3
  res$lower <- (sim$lower + hol$lower + dam$lower) / 3

  return(res)
}

#' Wrapper to preprecess, predict and postprocess forecasts
#'
#' @param fcs.fun forecasting function
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param pp.operations preprocessing operations to be conducted
#' @param ... not used
#'
#' @return forecast object
#' @export
generic_forecast <- function(fcs.fun, y, h, level, pp.operations = c('seasonal_adjustment'), ...) {
  # lade forecast
  require(forecast)
  # transform
  pp <- preprocessor()
  pp_y <- pp(y = y, action = 'transform', operations = pp.operations, frequency = frequency(y), h = h, ...)
  # predict
  y_hat <- get(fcs.fun, envir = environment(forecast))(y = y, h = h, level = level, ...)
  # inverse transformation for point forecast
  y_hat$mean <- pp(y = y_hat$mean, action = 'backtransform')
  y_hat$upper <- apply(y_hat$upper, 2, function(x) pp(y = x, action = 'backtransform'))
  y_hat$lower <- apply(y_hat$lower, 2, function(x) pp(y = x, action = 'backtransform'))
  return(y_hat)
}



#' Theta fit
#' 
#' This method is inspired by the implementation of E. Spiliotis and V. Assimakopoulos (2017) / Forecasting & Strategy Unit - NTUA.
#'
#' @param y historical observation in form of a time series object
#' @param h number of horizons to predict
#' @param level prediction interval level, only one allowed.
#' @param theta theta parameter.
#' @param curve curve type.
#' @param model model type.
#' @param seasonality seasonality type.
#'
#' @return point forecasts including one prediction interval level
#' @export
theta_fit <-
  function(y,
           h,
           level = 95,
           theta,
           curve,
           combination,
           seasonality) {
    #print(y)
    #Used to fit a Theta combination
    if (length(level) > 1) {
      level <- max(level)
      warning('Only one level supported so far, taking the highest level...')
    }

    #Check if the ys are valid
    if (theta < 0) {
      theta <- 2
    }
    if (h < 1) {
      h <- 1
    }

    #Estimate seasonaly adjusted time series
    pp <- preprocessor()
    des_y <- pp(y = y, action = 'transform', operations = 'seasonal_adjustment', frequency = frequency(y), h = h, type = combination)

    #If negative values, force to linear combination
    if (min(des_y) <= 0) {
      curve <- "Lrl"
      combination <- "A"
    }
    
    # get theta line zero
    tl_zero <- theta_line_zero(y = des_y, h = h, curve = curve, level = level)
    # get theta line
    tl <- theta_line(y = des_y, tl_zero = tl_zero, theta = theta, combination = combination)
    # get theta line 2
    tl_two <- forecast::ses(y = tl, h = h, level = level)
    # combine theta lines to reach final forecasts
    tl_fcs <- combine_theta_lines(y = des_y, h = h, combination = combination, tl_zero = tl_zero, tl_two = tl_two, theta = theta)
    # upper
    upper <- (tl_zero$upper / tl_zero$mean) * tl_fcs$mean
    lower <- (tl_zero$lower / tl_zero$mean) * tl_fcs$mean

    tsp(upper) <- tsp(lower) <- tsp(tl_two$mean)

    #Estimete Theta line (theta)
    fitted <- pp(y = tl_fcs$fitted, action = 'backtransform')
    mean <- pp(y = tl_fcs$mean, action = 'backtransform')
    upper <- pp(y = upper, action = 'backtransform')
    lower <- pp(y = lower, action = 'backtransform')


    #Zero forecasts become positive
    mean[mean < 0] <- 0
    upper[upper < 0] <- 0
    lower[lower < 0] <- 0


    return(
      list(
        mean = mean,
        fitted = fitted,
        upper = upper,
        lower = lower
      )
    )
}

#' Theta forecasts
#'
#' @param y historical observation in form of a time series object
#' @param h number of horizons to predict
#' @param combination combination type
#' @param tl_zero theta line zero
#' @param tl_two theta line two
#' @param theta theta parameter
#'
#' @return point forecasts including one prediction interval
#' @export
combine_theta_lines <- function(y, h, combination, tl_zero, tl_two, theta) {
  # get frequency for reusability later on
  fq_y <- frequency(y)
  #Estimate theta line weights for tl zero and tl two
  if (theta == 0) {
    tl_two_weight <- 0
  } else{
    tl_two_weight <- (1 / theta)
  }
  tl_zero_weight <- (1 - tl_two_weight)
  # reconstruct forecast and fitted values
  if (combination=="A") {
    # additive combination, normal
    theta_fitted <-
      as.numeric(tl_two$fitted * tl_two_weight) + as.numeric(tl_zero$fitted * tl_zero_weight)
    theta_mean <-
      as.numeric(tl_two$mean * tl_two_weight) + as.numeric(tl_zero$mean * tl_zero_weight)
  } else if (combination == "M" &
             any(tl_two$fitted <= 0) &
             any(tl_two$mean <= 0) &
             any(tl_zero$fitted <= 0) &
             any(tl_zero$mean <= 0)) {
    # multiplicative combination
    theta_fitted <-
      (as.numeric(tl_two$fitted) ^ tl_two_weight) * (as.numeric(tl_zero$fitted) ^ tl_zero_weight)
    theta_mean <-
      (as.numeric(tl_two$mean) ^ tl_two_weight) * (as.numeric(tl_zero$mean) ^ tl_zero_weight)
  } else {
    # fall back to additive combination if 0 values
    combination <- "A"
    tl <- theta_line(y = y, tl_zero = tl_zero, theta = theta, combination = combination)
    tl_two <- forecast::ses(y = tl, h = h)
    theta_fitted <-
      as.numeric(tl_two$fitted * tl_two_weight) + as.numeric(tl_zero$fitted * tl_zero_weight)
    theta_mean <-
      as.numeric(tl_two$mean * tl_two_weight) + as.numeric(tl_zero$mean * tl_zero_weight)
  }
  return(
    list(
      combination = combination,
      fitted = ts(theta_fitted, start = min(time(y)), frequency = fq_y),
      mean = ts(theta_mean, start = max(time(y) + (1 / fq_y)), frequency = fq_y)
    )
  )
}

#' Theta line
#'
#' @param y historical observarions
#' @param tl_zero theta line zero
#' @param theta theta parameter
#' @param combination combination type
#'
#' @return theta line to be fitted by ses
#' @export
theta_line <- function(y, tl_zero, theta, combination) {
  # combine the methods to get the theta line
  if (combination == "M" & all(tl_zero$fitted > 0) & all(tl_zero$mean > 0)) {
    theta_line <- (y ^ theta) * (tl_zero$fitted ^ (1 - theta))
  } else{
    combination <- "A"
    theta_line <- theta * y + (1 - theta) * tl_zero$fitted
  }
  return(theta_line)
}


#' Theta line zero
#'
#' @param y historical observations
#' @param h horizons to predict
#' @param curve curve type
#' @param level prediction interval level
#' @param ... 
#'
#' @return thea line zero
#' @export
theta_line_zero <- function(y, h, curve, level, ...) {
  #prepare data
  fq_y <- frequency(y)
  hist_x <- seq.int(length.out = length(y))
  fore_x <- (max(hist_x) + 1):(max(hist_x) + h)
  hist_df <- data.frame(y = y, x = hist_x)
  fore_df <- data.frame(x = fore_x)
  # estimate theta line based on curve type
  if (curve == "E") {
    estimate <- lm(log(y) ~ x, data = hist_df)
    # predict theta line 0
    in_sample <- exp(predict(estimate))
    out_sample <- exp(predict(estimate, fore_df, interval = 'predict', level = (level / 100)))
  } else{
    estimate <- lm(y ~ poly(x, 1, raw = TRUE), data = hist_df)
    # predict theta line 0
    in_sample <- predict(estimate)
    out_sample <- predict(estimate, fore_df, interval = 'predict', level = (level / 100))
  }

  # return list of results
  return(
    list(
      fitted = ts(in_sample, start = min(time(y)), frequency = fq_y),
      mean = ts(out_sample[, 1], start = max(time(y) + (1 / fq_y)), frequency = fq_y),
      lower = ts(out_sample[, 2], start = max(time(y) + (1 / fq_y)), frequency = fq_y),
      upper = ts(out_sample[, 3], start = max(time(y) + (1 / fq_y)), frequency = fq_y)
    )
  )
}

#' theta forecast
#'
#' @param y historical observations
#' @param h horizons to be predicted
#' @param level prediction interval level
#' @param model model to be predicted
#'
#' @return forecast object
#' @export
theta_forecast <- function(y, h, level = 95, model = "MEM") {
  #Used to automatically select the best Theta combination
  
  # ignore case
  model <- toupper(model)
  
  if (nchar(model) == 2) {
    warning('Assuming no seasonal component provided')
    # seasonality will be ignored anyway later in the preprocessing phase
    model <- paste('M', model, sep = '')
  } else if (nchar(model) != 3) {
    warning('Parameter model expects 3 letters; in form of Seasonality (M or A), Curve (E or L) and combination (M or A), defaulting to MEM')
    model <- 'MEM'
  }
  
  # extract seasonal components
  seasonality <- substr(model, 1, 1)
  # check for invalid input
  if (!(seasonality %in% c('M', 'A'))) {
    warning(paste(seasonality, 'is not a valid seasonal decomponsition method; select either M or A. Defaulting to M.'))
    seasonality <- 'M'
  }
  # check for curve
  curve <- substr(model, 2, 2)
  # check for invalid input
  if (!(curve %in% c('E', 'L'))) {
    warning(paste(curve, 'is not a valid curve mode; select either E or L. Defaulting to E.'))
    seasonality <- 'E'
  }
  # check for combination method
  combination <- substr(model, 3, 3)
  # check for invalid input
  if (!(combination %in% c('M', 'A'))) {
    warning(paste(combination, 'is not a valid combination mode; select either M or A. Defaulting to M.'))
    seasonality <- 'M'
  }
  
  
  #Scale
  base <- mean(y)
  y <- y / base

  #With this function determine opt theta per case
  optfun <- function(x, y = y, h = h, curve = curve, combination = combination, seasonality = seasonality) {
    mean(abs(
      theta_fit(
        y = y,
        h = h,
        theta = x,
        curve = curve,
        combination = combination,
        seasonality = seasonality
      )$fitted - y
    ))
  }
  optimized_theta <-
    optimize(
      optfun,
      c(1:3),
      y = y,
      h = h,
      curve = curve,
      combination = combination,
      seasonality = seasonality
    )$minimum

    theta_fcs <-
      theta_fit(
        y = y,
        h = h,
        level = level,
        theta = optimized_theta,
        curve = curve,
        combination = combination,
        seasonality = seasonality
      )

    res <- list(
      fitted = theta_fcs$fitted * base,
      mean = theta_fcs$mean * base,
      upper = theta_fcs$upper * base,
      lower = theta_fcs$lower * base
    )

  #Returns the fitted and forecasted values, as well as the combination used (Type of seasonality, Type of combination, Type of Trend, Theta coef.)
  return(res)
}

#' Auto Tehta
#'
#' Combination of univariate theta methods, using a combination operator of choice for the mean forecasts and prediction intervals.
#'
#' @param y Time series object with historical values.
#' @param h Horizons to be predicted.
#' @param levels Prediction interval levels. Optional, default \code{c(95)}.
#' @param methods Methods to be combined. Optional, default \code{auto_ets, auto_arima, auto_dotm}.
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
#' @param ... passed to the forecasting functions
#'
#' @return combined forecasts of time series y including confidence intervals
#' @export
auto_theta <- function(y,
                       h,
                       level = 95,
                       point_combination = 'weighted_average',
                       pi_combination_upper = 'median',
                       pi_combination_lower = 'median',
                       pool_limit = 3,
                       error_fun = 'rmse',
                       weight_fun = 'inverse',
                       val_h = h,
                       sov_only = F,
                       max_years = 30,
                       val_min_years = 4,
                       cv_min_years = 5,
                       cv_max_samples = 3,
                       allow_negatives = F,...) {
  
  # determine model pool
  if(is_seasonal(y = y, frequency = frequency(y))) {
    methods <- paste('theta', 
                   c('MEM', 'MEA', 'AEM', 'AEA', 
                     'MLM', 'MLA', 'ALM', 'ALA'),
                   sep = '_')
  } else {
    methods <- paste('theta', 
                    c('MEA', 'MEM', 'MLA', 'MLM'),
                    sep = '_')
  }
  
  methods <- tolower(methods)

  res <- tricomb(y = y,
                 h = h,
                 level = level,
                 methods = methods,
                 point_combination = point_combination,
                 pi_combination_upper = pi_combination_upper,
                 pi_combination_lower = pi_combination_lower,
                 pool_limit = pool_limit,
                 error_fun = error_fun,
                 weight_fun = weight_fun,
                 val_h = val_h,
                 sov_only = sov_only,
                 max_years = max_years,
                 val_min_years = val_min_years,
                 cv_min_years = cv_min_years,
                 cv_max_samples = cv_max_samples,
                 allow_negatives = allow_negatives)
  
  return(res)
}

# very ugly, but needed for combination
#' MEM theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_mem <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "MEM"))
}

#' MEA theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_mea <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "MEA"))
}

#' AEM theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_aem <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "AEM"))
}

#' AEA theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_aea <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "AEA"))
}

#' MLM theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_mlm <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "MLM"))
}

#' MLA theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_mla <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "MLA"))
}

#' ALM theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_alm <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "ALM"))
}

#' ALM theta model
#'
#' @param y historical values
#' @param h number of horizons to predict
#' @param level prediction interval levels
#' @param ... not used
#'
#' @return forecast object
#' @export
theta_ala <- function(y, h, level, ...) {
  return(theta_forecast(y = y, h = h, level = level, model = "ALA"))
}



