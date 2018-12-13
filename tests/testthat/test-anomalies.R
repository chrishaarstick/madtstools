
# Anomalies Unit Tests ----------------------------------------------------

# R Libraries
library(madtstools)
library(testthat)
library(checkmate)
library(lubridate)
library(dplyr)

context("anomalies unit tests")


# Create simulated data
set.seed(319)
n <- 100
df <- tibble(index = 1:n,
             x = as.numeric(arima.sim(model = list(0,0,0), n)))

# Add Anomalies
an <- 2
pos_anoms_indx <- sample(1:n, an, replace = F)
df$x[pos_anoms_indx] <- runif(an, 4, 6)

neg_anoms_indx <- sample(setdiff(1:n, pos_anoms_indx), an, replace = F)
df$x[neg_anoms_indx] <- runif(an, -6, -4)



# Unit Tests --------------------------------------------------------------

test_that("postive anomalies identified", {
  anom_df <- anomalies(df, "index", "x", ts_frequency = 5,
                       confidence = .99, direction = "positive")
  anom_indx <- anom_df %>% 
    filter(anomaly == 1) %>% 
    pull(index)
  
  expect_data_frame(anom_df)
  expect_subset(pos_anoms_indx, anom_indx)
})



test_that("negative anomalies identified", {
  anom_df <- anomalies(df, "index", "x", ts_frequency = 5,
                       confidence = .99, direction = "negative")
  anom_indx <- anom_df %>% 
    filter(anomaly == 1) %>% 
    pull(index)
  
  expect_data_frame(anom_df)
  expect_subset(neg_anoms_indx, anom_indx)
})



test_that("both direction anomalies identified", {
  anom_df <- anomalies(df, "index", "x", ts_frequency = 5,
                       confidence = .99, direction = "both")
  anom_indx <- anom_df %>% 
    filter(anomaly == 1) %>% 
    pull(index)
  
  expect_data_frame(anom_df)
  expect_subset(c(pos_anoms_indx, neg_anoms_indx), anom_indx)
})


test_that("confidence threshold works as expected", {
  
  anom_df1 <- anomalies(df, "index", "x", ts_frequency = 5,
                        confidence = .95, direction = "both")
  
  anom_df2 <- anomalies(df, "index", "x", ts_frequency = 5,
                        confidence = .99, direction = "both")
  
  expect_gte(
    nrow(dplyr::filter(anom_df1, anomaly == 1)),
    nrow(dplyr::filter(anom_df2, anomaly == 1))
  )
})