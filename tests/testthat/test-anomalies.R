
# Anomalies Unit Tests ----------------------------------------------------

# R Libraries
library(madtstools)
library(testthat)
library(checkmate)
library(tidyverse)
library(lubridate)


context("anomalies unit tests")


# Create simulated data
set.seed(319)
n <- 100
df <- tibble(index = 1:n,
             x = arima.sim(model = list(0,0,0), n))

# Add Anomalies
an <- 2
pos_anoms_indx <- sample(1:n, an, replace = F)
df$x[pos_anoms_indx] <- runif(an, 4, 6)

neg_anoms_indx <- sample(setdiff(1:n, pos_anoms_indx), an, replace = F)
df$x[neg_anoms_indx] <- runif(an, -6, -4)



# Unit Tests --------------------------------------------------------------

test_that("postive anomalies identified", {
  anoms <- anomalies(df, "index", "x", stl_args = list(), confidence = .99, direction = "positive")
  
  expect_data_frame(anoms)
})
