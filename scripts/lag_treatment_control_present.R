## Collider Bias 1 in TSCS

library(dagitty)
library(rethinking)
library(ggdag)
library(tidyr)
library(tidyverse)
library(statar)
library(fixest)


lag_treatment <- ggdag::dagify(y2 ~ y1,
                         w1 ~ y1,
                         w1 ~ x1,
                         x2 ~ x1,
                         y2 ~w1,
                         )

ggdag::ggdag(lag_treatment)

# simulates from DAG- 30 periods

set.seed(1234)
n <- 200
beta <- c(.5, .5, 1.5, -1.5, 1.5)

causalx <- function(n = 200, beta = c(.5, .5, 1.5, -1.5, 1.5), num_periods = 30) {
  epsilon <- matrix(rnorm(n*num_periods), nrow = n, ncol = num_periods)
  delta <- matrix(rnorm(n*num_periods), nrow = n, ncol = num_periods)
  gamma <- matrix(rnorm(n*num_periods), nrow = n, ncol = num_periods)
  x <- matrix(0, nrow = n, ncol = num_periods)
  y <- matrix(0, nrow = n, ncol = num_periods)
  w <- matrix(0, nrow = n, ncol = num_periods)
  x[,1] <- rnorm(n)
  y[,1] <- rnorm(n)
  w[,1] <-  x[,1]*beta[4] + y[,1]*beta[5] + delta[,1]

  
 
  for ( i in 2:num_periods) {
    x[,i] <- x[,i-1]*beta[1] + epsilon[,i]
    y[,i] <- y[,i-1]*beta[2] + w[,i-1]*beta[3]  + gamma[,i]
    w[,i] <- x[,i]*beta[4] + y[,i]*beta[5] + delta[,i]
  }
  
  x <- as.data.frame(x)
  x <- x %>%
    mutate(id = 1:n()) %>%
    pivot_longer(!id, names_to = "period", values_to = "x", names_prefix = "V")
  
  y <- as.data.frame(y) %>%
    mutate(id = 1:n()) %>%
    pivot_longer(!id, names_to = "period", values_to = "y", names_prefix = "V")
  
  w <- as.data.frame(w) %>%
    mutate(id = 1:n()) %>%
    pivot_longer(!id, names_to = "period", values_to = "w", names_prefix = "V")
  
  df <- x %>%
    inner_join(y, by = c("id", "period")) %>%
    inner_join(w, by = c("id", "period")) %>%
    mutate(period = as.integer(period)) %>%
    group_by(id) %>%
    mutate(x_lag = tlag(x, n=1, period),
           w_lag = tlag(w, n=1, period),
           y_lag = tlag(y, n=1, period)) %>%
  ungroup() %>%
    filter(!is.na(x_lag))
  
  return(df)
}

reg <- lm(y ~ x, data=df)
summary(reg)

reg <- lm(y ~ x_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag + w_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag  + y_lag, data=df)
summary(reg)

reg <- feols(y ~ x_lag + w_lag | id + period, data=df)
summary(reg)

beta1 <- 2
beta2 <- 1.5
beta3 <- -1
beta4 <- -1.5
beta5 <- -2
x0 <- rnorm(n)
y0 <- rnorm(n)
x2 <- beta1*x1 + rnorm(n)
w1 <- beta2*x1 + beta3*y1 + rnorm(n)
y2 <- beta4*y1 + beta5*w1
w0 <- 

df <- data.frame(x1 = x1, x2 = x2, w1 = w1, y1 = y1, y2 = y2)

reg <- lm(y2 ~ x2, data=df)
summary(reg)


