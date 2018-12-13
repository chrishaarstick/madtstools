
# Anomalies function ------------------------------------------------------

#' Anomaly Indentification
#'
#' Function to idendtify anomalies in time series data. Function similar to
#' Seasonal ESD approach here : \url{https://arxiv.org/pdf/1704.07706.pdf}
#'
#' @param df input data.frame
#' @param index_var optional column name of index variable. default is NULL
#'   which uses the first column in the input data.frame
#' @param x_var optional column name of numeric variable to analyze. efault is
#'   NULL which uses the second column in the input data.frame
#' @param ts_frequency time series frequency. default is 1
#' @param stl_args list of arguments to pass to \link[stats]{stl} function
#' @param confidence anomaly threshold confidence level. higher levels increase
#'   the anomaly threshold
#' @param direction direction of anomaly idendification. Options are `positive`,
#'   `negative`, or `both`
#'
#' @return data.frame with input dataset, stl components and flag for anomalies
#' @export
#' @examples
#' library(tidyverse)
#' set.seed(319)
#'  n <- 100
#'   df <- tibble(index = 1:n,
#'                x = as.numeric(arima.sim(model = list(0,0,0), n),
#'                                         rand.gen = function(x) rt(x, 4)))
#'
#' anoms <- anomalies(df, "index", "x", stl_list = list(), confidence = 0.95,
#' direction = "both")
anomalies <- function(df,
                      index_var = NULL,
                      x_var = NULL,
                      ts_frequency = 1,
                      stl_args = list(s.window = "periodic"),
                      confidence = 0.99,
                      direction = "both") {
  
  checkmate::assert_data_frame(df, min.cols = 2, min.rows = 10)
  checkmate::assert_string(index_var, na.ok = TRUE)
  checkmate::assert_string(x_var, na.ok = TRUE)
  checkmate::assert_number(ts_frequency, lower = 1)
  checkmate::assert_list(stl_args)
  checkmate::assert_number(confidence, lower = .8, upper = .999)
  checkmate::assert_choice(direction, choices = c("positive", "negative", "both"))
  
  
  # Extract values
  if(is.null(x_var)) x <- df[[2]] else x <- df[[x_var]]
  
  if(is.null(index_var)) index <- df[[1]] else index <- df[[index_var]]
  
  
  # stl decomp
  x_ts <- ts(data = x, frequency = ts_frequency)
  x_stl <- do.call("stl", args = modifyList(stl_args, list(x = x_ts)))
  
  
  # extract components
  x_anoms <- dplyr::bind_cols(
    dplyr::select_at(df, c(index_var, x_var)), 
    tibble::as.tibble(x_stl$time.series))
  
  
  # standarize residual
  x_anoms <- dplyr::mutate(x_anoms, 
                           resid = (remainder - median(remainder))/mad(remainder))
  
  
  # get anomaly thresold
  pval <- ifelse(direction == "both", 1 - (1-confidence) / 2, confidence)
  thresh <- qnorm(pval) 
  
  
  # indentify anomalies and return result
  dplyr::mutate(x_anoms, 
         anomaly = dplyr::case_when(direction == "both" ~ ifelse(abs(resid) > thresh, 1, 0),
                                    direction == "negative" ~ ifelse(resid < -thresh, 1, 0),
                                    TRUE ~ ifelse(resid > thresh, 1, 0)))
}
