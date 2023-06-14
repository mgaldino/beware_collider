## sim RQ


library(ggdag)
library(arm)
library(tidyr)
library(tidyverse)
library(statar)
library(fixest)
library(pdynmc)
library(plm)

# fristly, let's plot the DAG that will be simulated
lag_treatment <- ggdag::dagify(cw2 ~ cw1 + pchange1 + incomepc1,
                               cw1 ~ cw0 + pchange0 + incomepc0,
                               pchange1 ~ pchange0 + incomepc1,
                               incomepc1 ~ incomepc0,
                               pchange0 ~ incomepc0
)

ggdag::ggdag(lag_treatment, text_col = "red", label_col = "white")

n <- 1000
time <- 10
# init variables
cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
incomepc0 <- 1000*exp(rnorm(n)) # log normal

# period 0 variables
pchange0 <- rbinom(n, 1, p=invlogit(-.01*incompe0 + rnorm(n, 0, 10)))

# period 1 variables
incomepc1 <- incompe0 + incompe0*runif(n, .95, 1.1)
pchange1 <- rbinom(n, 1, p=invlogit(-.01*incomepc1 + 10*pchange0 + rnorm(n, 0, 10)))
cw1 <- 5 + cw0 + 5*pchange0 - log(incomepc0) + rnorm(n, 0, 3)

# period 2
cw2 <- 5 + cw1 + 5*pchange1 - log(incomepc1) + rnorm(n, 0, 3)

df <- data.frame(cw = c(cw2, cw1), pchange = c(pchange0, pchange1), 
                 incomepc = c(incomepc0, incomepc1), period = rep(1:2, each= n), id = 1:n)

summary(df)
reg <- lm(cw ~ pchange + log(incomepc), df)
summary(reg)

