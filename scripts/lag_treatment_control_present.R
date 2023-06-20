## Collider Bias 1 in TSCS

library(ggdag)
library(tidyr)
library(tidyverse)
library(statar)
library(fixest)
library(pdynmc)
library(plm)

# fristly, let's plot the DAG that will be simulated
lag_treatment <- ggdag::dagify(y2 ~ y1,
                         w1 ~ y1,
                         w1 ~ x1,
                         x2 ~ x1,
                         y2 ~w1,
                         y2 ~ x1
                         )

ggdag::ggdag(lag_treatment)

# simulates from DAG- 30 periods

set.seed(1234)
n <- 1000
beta <- c(.5, .5, .5, .5, .5, 2) # direct effect of x_t-1 on y is 2. All other effects are .5.

# function to simulate from the above DAG
causal_sim1 <- function(n = 200, beta = c(.5, .5, .5, .5, .5, 2), num_periods = 30) {
 
  epsilon <- matrix(rnorm(n*num_periods, 0 ,1), nrow = n, ncol = num_periods)
  delta <- matrix(rnorm(n*num_periods, 0 ,2), nrow = n, ncol = num_periods)
  gamma <- matrix(rnorm(n*num_periods, 0 ,2), nrow = n, ncol = num_periods)
  
  x <- matrix(0, nrow = n, ncol = num_periods)
  y <- matrix(0, nrow = n, ncol = num_periods)
  w <- matrix(0, nrow = n, ncol = num_periods)
  x[,1] <- rnorm(n)
  y[,1] <- rnorm(n)
  w[,1] <-  x[,1]*beta[4] + y[,1]*beta[5] + delta[,1]

  
 
  for ( i in 2:num_periods) {
    x[,i] <- x[,i-1]*beta[1] + epsilon[,i]
    y[,i] <- y[,i-1]*beta[2] + w[,i-1]*beta[3]  + x[,i-1]*beta[6] +  gamma[,i]
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

df <- causal_sim1(beta= beta)

# now, showing that the direct effect of x on y is not identified (should be 2)
reg <- lm(y ~ x, data=df)
summary(reg)

reg <- lm(y ~ w_lag, data=df)
summary(reg)

reg <- lm(y ~ x + x_lag + w_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag, data=df)
summary(reg)

reg <- lm(y ~ x + x_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag + w_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag  + y_lag, data=df)
summary(reg)

reg <- lm(y ~ x_lag  + y_lag + w_lag, data=df)
summary(reg) ## why NA? Why perfectly correlated?

# it does not make any difference using fixed effects
reg <- feols(y ~ x_lag + w_lag | id + period, data=df)
summary(reg)

reg <- feols(y ~ x_lag + w_lag + y_lag| id + period, data=df)
summary(reg)

reg <- lm(y ~ x_lag  + w_lag, data= subset(df, period == 2))
summary(reg)

reg <- lm(y ~ x_lag , data= subset(df, period == 3))
summary(reg)

reg <- lm(y ~ x_lag +w_lag, data= subset(df, period == 3))
summary(reg)

reg <- pdynmc(dat = df, varname.i = "id", varname.t = "period",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "y", lagTerms.y = 1,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("x", "w"), lagTerms.reg.fur = c(1,1),
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "none")

summary(reg)

panel_one_step_gmm <- pgmm(y ~  x + w | lag(y, 2) + lag(x, 2), lag.form(1, 1, 1),
                           data = df,
                           transformation = 'd',
                           model = 'onestep',
                           effect = 'individual', lag.gmm = list(c(2,3)))

summary(panel_one_step_gmm)
