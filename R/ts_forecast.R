#' Wrapper to forecast with RevoscaleR
#' 
#' Create a wrapper around forunco that takes data from sql server or spark and 
#' predicts the future timesteps with forunco logic.
#'
#' @param keys key of the time series, integer
#' @param data data of the time series passed by rxExecBy
#' @param params function parameters by rxExecBy
#'
#' @return forunco object
".rx_forecast" <- function(keys, 
                           data, 
                           h,
                           levels,
                           methods,
                           pp_methods,
                           point_combination,
                           pi_combination_upper,
                           pi_combination_lower,
                           pool_limit,
                           error_fun,
                           weight_fun,
                           val_h,
                           sov_only,
                           max_years,
                           val_min_years,
                           cv_min_years,
                           cv_max_samples,
                           allow_negatives,
                           ...) {
  # get data --> SELECT * FROM Target WHERE KeyCol = Key
  inputData <- rxImport(inData = data)
  # order data.frame by date
  inputData <- inputData[order(as.Date(inputData$Date)),]
  # data to date_int
  date_int <- lubridate::decimal_date(as.Date(inputData$Date))
  # create time series object
  y <- ts(data = as.vector(inputData$Value), end = tail(date_int, 1), frequency = unique(inputData$Frequency))
  # create 
  forunco <- forunco::forunco(
    y = y, 
    h = h,
    levels = levels,
    methods = methods,
    pp_methods = pp_methods,
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
    allow_negatives = allow_negatives
  )
  return(forunco)
}


#' forunco function for SQL Server compute context
#' 
#' Implements the forunco function for optimal parallelisation with machine learning services of sql server 2017
#'
#' @param connection_string mandatory: Connectionstring to the database
#' @param table mandatory: Table name that is the source for the forecasting objects
#' @param num_cores number of cores to be used, default \code{8}.
#' @param h number of horizons to predict.
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
#' @param ... 
#'
#' @return forecest data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' connection_string <- "Server=LTYMA01\\QWERTZ;Database=FORECASTING_DATA;UID=ruser;PWD=ruser;"
#' table <- paste0("m3.vw_Micro")
#' result <- rx_sql_forunco(connection_string=connection_string, table=table)
#' #    .id  TSN       date  type       value
#' #     P1 1403 1994-03-02 Point  1468.48451
#' #     P1 1403 1994-03-30 Point  1468.48451
#' #     P1 1403 1994-04-30 Point  1468.48451
#' #     P1 1403 1994-05-30 Point  1468.48451
#' #     P1 1403 1994-06-30 Point  1468.48451
#' }
rx_sql_forunco <- function(connection_string, 
                           table, 
                           num_cores = 8,
                           h = 2,
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
                           # tbd: additional parameters 
                           # of sql server have 
                           ...) {
  require(plyr)
  require(dplyr)
  if (!(RevoscaleR %in% installed.packages())) {
    stop("This function can only run on SQL 
    ML Services or MRO Client with the Revoscaler package")
  }

  # define SQL Server Data source
  sqlServerDataDS <- RxSqlServerData(table = table, connectionString = connection_string)
  
  # Set SQL Server compute context with level of parallelism = 2
  sqlServerCC <- RxInSqlServer(
    connectionString = connection_string, 
    numTasks = 1, 
    packagesToLoad = c('forunco', 'dplyr')
  )
  # set most important options
  rxOptions(numCoresToUse=num_cores)
  # set compute context
  rxSetComputeContext(sqlServerCC)
  params = list(
    h = h,
    levels = levels,
    methods = methods,
    pp_methods = pp_methods,
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
    allow_negatives = allow_negatives
  )
  # execute prediction function
  qlServerCCResults <- rxExecBy(
    inData = sqlServerDataDS, 
    keys = c("TSN"), 
    func = .rx_forecast,
    funcParams = params
  )
  
  res <- lapply(qlServerCCResults, function(x) {
    data.frame("TSN"=x$keys[[1]][[1]], x$result$preds)
  })
  # return result
  res <- plyr::ldply(res, rbind) %>% 
    dplyr::mutate(date = as.character(date))
  
  return(res)
}


#' Forunco function for batch forecasting in R
#' 
#' Implements the forunco function for optimal parallelisation with machine learning services of sql server 2017
#'
#' @param ts_col list of time series objects 
#' @param h number of horizons to predict
#' @param num_cores number of cores to be used; default: NULL (all cores)
#' @param num_cores_ignore number of cores to be ignored for e.g. OS; default: 1
#' @param prog_bar boolean, whether or not a progress bar should be displyed
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
#' @param ... 
#'
#' @return list with mean, pis and data_frame containing the predicted values
#' @export
#'
#' @examples
#' \dontrun{
#' library(forunco)
#' library(Mcomp)
#' m3 <- M3[2000:2025]
#' #a time series colllection has n elements, each of which is a time series
#' # object.
#' ts_col <- lapply(m3, function(x) {x$x})
#' 
#' result <- forecast_forunco(ts_col)
#' preds[[1]]
#' $mean
#' [1] 5054.439 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170 5048.170
#' 
#' $upper
#' [1] 6348.204 6883.349 7294.081 7640.378 7945.488 8221.338 8475.014 8711.134 8932.906 9142.665 9342.174 9532.805
#' 
#' $lower
#' [1] 3760.6740 3149.1485 2453.7666 2132.0499 2054.5814 1887.5394 1633.8633 1397.7433 1175.9717  894.6316  766.7032  576.0728
#' 
#' $preds
#' # A tibble: 36 X 3
#' date  type    value
#' <date>     <chr>    <dbl>
#' 1989-07-02 Point 5054.439
#' 1989-08-02 Point 5048.170
#' 1989-09-02 Point 5048.170
#' 1989-10-02 Point 5048.170
#' 1989-11-02 Point 5048.170
#' 1989-12-02 Point 5048.170
#' 1990-01-02 Point 5048.170
#' 1990-02-02 Point 5048.170
#' 1990-03-02 Point 5048.170
#' 1990-04-02 Point 5048.170
# ... with 26 more rows
#' }
forecast_forunco <- function(ts_col, 
                             h = 12,
                             num_cores = NULL,
                             num_cores_ignore = 1,
                             prog_bar = T,
                             levels = c(95),
                             methods = c('auto_ets', 'auto_arima', 'auto_dotm'),
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
                             # tbd: additional parameters 
                             # of sql server  
                             ...) {
  # load packages
  #require(forunco)
  require(foreach)
  require(doSNOW)
  require(parallel)
  require(progress)
  
  # create progress bar object
  if (prog_bar) {
    # progress bar object
    pb <- progress_bar$new(
      format = "  [:bar] :percent eta: :eta",
      total = length(ts_col),
      clear = F,
      force = T
    )
    prop_progress <- function() { pb$tick() }
  } else {
    prop_progress <- function() { NULL }
  }

  # set num_cores if it is not set by the user
  if (is.null(num_cores))
    num_cores <- detectCores()
  

  cl <- makeCluster(num_cores - num_cores_ignore, type = "SOCK")
  registerDoSNOW(cl)
  clusterEvalQ(cl, { 
    library(forunco); 
    library(dplyr); 
    library(forecast)
  })
  #clusterExport(cl, ls())
  
  #
  tc <- foreach (i = seq.int(length.out = length(ts_col)), .options.snow = list(progress=prop_progress)) %dopar% {
    ts <- ts_col[[i]]
    forunco(y = ts,
            h = h,
            levels = levels,
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
            allow_negatives = allow_negatives
    )
  }
  stopCluster(cl)
  return(tc)
}