
# Anomalies function ------------------------------------------------------

#' Title
#'
#' @param df input data.frame
#' @param index optional column name of index variable. default is NULL which
#'   uses the first column in the input data.frame
#' @param x optional column name of numeric variable to analyze. efault is NULL
#'   which uses the second column in the input data.frame
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
#' n <- 100
#' df <- tibble(index = 1:n,
#'             x = arima.sim(model = list(0,0,0), n), rand.gen = function(x) rt(x, 4))
#'
#' anoms <- anomalies(df, "index", "x", stl_list = list(), confidence = 0.95, direction = "both")
anomalies <- function(df,
                      index = NULL,
                      x = NULL,
                      stl_args = list(),
                      confidence = 0.99,
                      direction = "both") {
  
  checkmate::assert_data_frame(df, min.cols = 2, min.rows = 10)
  checkmate::assert_string(index, na.ok = TRUE)
  checkmate::assert_string(x, na.ok = TRUE)
  checkmate::assert_list(stl_args)
  checkmate::assert_number(confidence, lower = .8, upper = .999)
  checkmate::assert_choice(direction, choices = c("positive", "negative", "both"))
  
  
  df
}
