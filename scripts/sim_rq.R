## sim RQ


library(ggdag)
library(arm)
library(tidyr)
library(tidyverse)
# library(statar)
library(fixest)
# library(pdynmc)
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
cw1 <- 5 + cw0 + 5*pchange0 - 1.1*log(incomepc0) + rnorm(n, 0, 3)

# period 2
cw2 <- 5 + cw1 + 5*pchange1 - 1.1*log(incomepc1) + rnorm(n, 0, 3)

df <- data.frame(cw = c(cw2, cw1), pchange = c(pchange0, pchange1), 
                 incomepc = c(incomepc0, incomepc1), period = rep(1:2, each= n), id = 1:n)

df <- df %>%
  mutate(cw_new = cw)
summary(df)
reg <- lm(cw ~ pchange + log(incomepc), df)
summary(reg)

# tratamento heterogêneo
# fristly, let's plot the DAG that will be simulated
lag_treatment <- ggdag::dagify(cw2 ~ cw1 + pchange1 + incomepc1,
                               cw1 ~ cw0 + pchange0 + incomepc0,
                               pchange1 ~ pchange0 + incomepc1,
                               incomepc1 ~ incomepc0,
                               pchange0 ~ incomepc0
)

ggdag::ggdag(lag_treatment, text_col = "red", label_col = "white")

estimate <- 0
std_error <- 0
# simulando 100 ordens diferentes de tratamento

for (i in 1:100) {
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
  cw1 <- 5 + cw0 + 5*pchange0 - 1.1*log(incomepc0) + rnorm(n, 0, 3)
  
  # period 2
  cw2 <- 5 + cw1 + 2.5*pchange1 - 1.1*log(incomepc1) + rnorm(n, 0, 3)
  
  df <- data.frame(cw = c(cw2, cw1), pchange = c(pchange0, pchange1), 
                   incomepc = c(incomepc0, incomepc1), period = rep(1:2, each= n), id = 1:n)
  
  summary(df)
  
  reg <- lm(cw ~ pchange + log(incomepc), df)
  
  summary(reg)
  estimate[i] <- coef(reg)[2]
  std_error[i] <- sqrt(diag(vcov(reg)))[2]
  print(i)
  
}

df_sim <- data.frame(iteration = 1:100, coef = estimate,std_error = std_error )
library(ggplot2)
library(tidyverse)

library(ggplot2)

p <- df_sim %>%
  ggplot(aes(x = iteration, y = coef)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef - std_error, ymax = coef + std_error), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression Coefficient Estimates with Error Bars")
  
ggsave(p, filename = "coef_sim_het_effect.png")


## RQ homogêneo
estimate <- 0
std_error <- 0
for (i in 1:100) {
  n <- 10000
  time <- 10
  # init variables
  cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
  incomepc0 <- 1000*exp(rnorm(n)) # log normal
  
  # period 0 variables
  pchange0 <- rbinom(n, 1, p=invlogit(-.01*incompe0 + rnorm(n, 0, 10)))
  
  # period 1 variables
  incomepc1 <- incompe0 + incompe0*runif(n, .95, 1.1)
  pchange1 <- rbinom(n, 1, p=invlogit(-.01*incomepc1 + 10*pchange0 + rnorm(n, 0, 10)))
  cw1 <- 5 + cw0 + 5*pchange0 - 1.1*log(incomepc0) + rnorm(n, 0, 3)
  
  # period 2
  cw2 <- 5 + cw1 + 5*pchange1 - 1.1*log(incomepc1) + rnorm(n, 0, 3)
  
  df <- data.frame(cw = c(cw2, cw1), pchange = c(pchange0, pchange1), 
                   incomepc = c(incomepc0, incomepc1), period = rep(1:2, each= n), id = 1:n)
  
  summary(df)
  reg <- lm(cw ~ pchange + log(incomepc), df)
  summary(reg)
  estimate[i] <- coef(reg)[2]
  std_error[i] <- sqrt(diag(vcov(reg)))[2]
  print(i)
}

df_sim <- data.frame(iteration = 1:100, coef = estimate,std_error = std_error )

p1 <- df_sim %>%
  ggplot(aes(x = iteration, y = coef)) +
  geom_point() + geom_smooth(method="lm") +
  ylim(0,6) +
  geom_errorbar(aes(ymin = coef - std_error, ymax = coef + std_error), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  geom_hline(yintercept = 5) +
  theme_bw() +
  ggtitle("Regression Coefficient Estimates with Error Bars")
p1
ggsave(p1, filename = "coef_sim_homog_effect.png")


## RQ 2 versus 0

for (i in 1:100) {
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
  cw1 <- 5 + cw0 + 5*pchange0 - 1.1*log(incomepc0) + rnorm(n, 0, 3)
  
  # period 2
  cw2 <- 5 + cw1 + 2.5*pchange1 - 1.1*log(incomepc1) + rnorm(n, 0, 3)
  
  df <- data.frame(cw = c(cw2, cw1), pchange = c(pchange0, pchange1), 
                   incomepc = c(incomepc0, incomepc1), period = rep(1:2, each= n), id = 1:n)
  

  df <- df %>%
    group_by(id) %>%
    mutate(somapol = sum(pchange)) %>%
    filter(somapol > 1 | somapol < 1)
  
  reg <- lm(cw ~ pchange + log(incomepc), df)
  summary(reg)
  estimate[i] <- coef(reg)[2]
  std_error[i] <- sqrt(diag(vcov(reg)))[2]
  print(i)
}

df_sim1 <- data.frame(iteration = 1:100, coef = estimate,std_error = std_error )

p2 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef - std_error, ymax = coef + std_error), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression Coefficient Estimates with Error Bars")

ggsave(p2, filename = "coef_sim_het_effect.png")

### Generalization ofr 10 periods
estimate <- numeric()
estimate1 <- numeric()
estimate2 <- numeric()
estimate3 <- numeric()

std_error <- numeric()
std_error1 <- numeric()
std_error2 <- numeric()
std_error3 <- numeric()

for (i in 1:100) {
  n <- 1000
  time <- 10
# init variables

cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
incomepc0 <- 1000*exp(rnorm(n)) # log normal

# creates vector of variables and initialize pchange0
pchange0<- rbinom(n, 1, p=invlogit(-.8*log(incompe0) + rnorm(n, 0, 10))) 
incomepc <- numeric()
cw <- numeric()
pchange <- numeric()
# period 1 variables
incomepc <- append(incomepc0, incompe0*runif(n, .95, 1.1))
pchange <- append(pchange0, rbinom(n, 1, p=invlogit(-.8*log(incomepc[1001:2000]) + 10*pchange0 + rnorm(n, 0, 8))))
cw <- append(cw0, 3 + cw0 + 5*pchange0 - log(incomepc0) + rnorm(n, 0, 2))

# 5 + cw0[1] + 5*pchange0[1] - .5*log(incomepc0[1]) + rnorm(1, 0, 3)
# 5 + cw0[2] + 5*pchange0[2] - .5*log(incomepc0[2]) + rnorm(1, 0, 3)

lag_index <- 1:n
for ( k in 1:time) {
  incomepc <- append(incomepc,  incomepc[lag_index + n*k]*runif(n, .95, 1.1))
  pchange <- append(pchange, rbinom(n, 1, p=invlogit(-.8*log(incomepc[lag_index + n*(k+1)]) +
                                                       10*pchange[lag_index + n*k] + rnorm(n, 0, 8))))
  cw <- append(cw, 3 + cw[lag_index + n*k] + 5*pchange[lag_index + n*k] - log(incomepc[lag_index + n*k]) +
                 rnorm(n, 0, 2))
}

df <- data.frame(cw = cw, pchange = pchange, 
                 incomepc = incomepc, period = rep(1:(time+2), each= n), id = rep(1:n, time+2)) %>%
  mutate(log_income =  log(incomepc)) %>%
  group_by(id) %>%
  mutate(lag_cw = dplyr::lag(cw, order_by=period),
         lag_pchange = dplyr::lag(pchange, order_by=period),
         lag_log_income = dplyr::lag(log_income, order_by = period)) %>%
  dplyr::filter(period != 1)


reg <- feols(cw ~ lag_pchange + lag_log_income + lag_cw, data = df)
summary(reg)

reg1 <- feols(cw ~ lag_pchange + lag_log_income, data = df)
summary(reg1)

reg_fe <- feols(cw ~ lag_pchange + lag_log_income + lag_cw  | id  + period, data = df)
summary(reg_fe)

reg_fe1 <- feols(cw ~ lag_pchange + lag_log_income  | id + period , data = df)
summary(reg_fe1)


estimate[i] <- coef(reg)[2]
estimate1[i] <- coef(reg1)[2]
estimate2[i] <- coef(reg_fe)[2]
estimate3[i] <- coef(reg_fe1)[2]

std_error[i] <- se(reg)[2]
std_error1[i] <- se(reg1)[2]
std_error2[i] <- se(reg_fe)[2]
std_error3[i] <- se(reg_fe1)[2]
print(i)
}

df_sim1 <- data.frame(iteration = 1:100, coef1 = estimate, coef2 = estimate1,
                      std_error1 = std_error, std_error2 = std_error1 )

p3 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef1)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef1 - std_error1, ymax = coef1 + std_error1), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression estimate of Political Change \n cw ~ pol_change + income + lag cw")

p4 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef2)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef2 - std_error2, ymax = coef2 + std_error2), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression estimate of Political Change - cw ~ pol_change + income")

ggsave(p3, filename = "coef_sim_homg_effect_lag_vd1.png")

ggsave(p4, filename = "coef_sim_homg_effect.png")

##################33
## Collider bias? #
#################3##

estimate <- numeric()
estimate1 <- numeric()
estimate2 <- numeric()
estimate3 <- numeric()

std_error <- numeric()
std_error1 <- numeric()
std_error2 <- numeric()
std_error3 <- numeric()

for (i in 1:100) {
  n <- 1000
  time <- 10
  # init variables
  
  cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
  incomepc0 <- 1000*exp(rnorm(n)) # log normal
  
  # creates vector of variables and initialize pchange0
  pchange0<- rbinom(n, 1, p=invlogit(-.8*log(incompe0) + rnorm(n, 0, 10))) 
  incomepc <- numeric()
  cw <- numeric()
  pchange <- numeric()
  # period 1 variables
  incomepc <- append(incomepc0, incompe0*runif(n, .95, 1.1) - .2*cw0 )
  pchange <- append(pchange0, rbinom(n, 1, p=invlogit(-.8*log(incomepc[1001:2000]) + 10*pchange0 + rnorm(n, 0, 8))))
  cw <- append(cw0, 3 + cw0 + 5*pchange0 - log(incomepc0) + rnorm(n, 0, 2))
  
  # 5 + cw0[1] + 5*pchange0[1] - .5*log(incomepc0[1]) + rnorm(1, 0, 3)
  # 5 + cw0[2] + 5*pchange0[2] - .5*log(incomepc0[2]) + rnorm(1, 0, 3)
  
  lag_index <- 1:n
  for ( k in 1:time) {
    incomepc <- append(incomepc,  incomepc[lag_index + n*k]*runif(n, .95, 1.1) - .2*cw[lag_index + n*k])
    pchange <- append(pchange, rbinom(n, 1, p=invlogit(-.8*log(incomepc[lag_index + n*(k+1)]) +
                                                         10*pchange[lag_index + n*k] + rnorm(n, 0, 8))))
    cw <- append(cw, 3 + cw[lag_index + n*k] + 5*pchange[lag_index + n*k] - log(incomepc[lag_index + n*k]) +
                   rnorm(n, 0, 2))
  }
  
  df <- data.frame(cw = cw, pchange = pchange, 
                   incomepc = incomepc, period = rep(1:(time+2), each= n), id = rep(1:n, time+2)) %>%
    mutate(log_income =  log(incomepc)) %>%
    group_by(id) %>%
    mutate(lag_cw = dplyr::lag(cw, order_by=period),
           lag_pchange = dplyr::lag(pchange, order_by=period),
           lag_log_income = dplyr::lag(log_income, order_by = period)) %>%
    dplyr::filter(period != 1)
  
  
  reg <- feols(cw ~ lag_pchange + lag_log_income + lag_cw, data = df)
  summary(reg)
  
  reg1 <- feols(cw ~ lag_pchange + lag_log_income, data = df)
  summary(reg1)
  
  reg_fe <- feols(cw ~ lag_pchange + lag_log_income + lag_cw  | id  + period, data = df)
  summary(reg_fe)
  
  reg_fe1 <- feols(cw ~ lag_pchange + lag_log_income  | id + period , data = df)
  summary(reg_fe1)
  
  
  estimate[i] <- coef(reg)[2]
  estimate1[i] <- coef(reg1)[2]
  estimate2[i] <- coef(reg_fe)[2]
  estimate3[i] <- coef(reg_fe1)[2]
  
  std_error[i] <- se(reg)[2]
  std_error1[i] <- se(reg1)[2]
  std_error2[i] <- se(reg_fe)[2]
  std_error3[i] <- se(reg_fe1)[2]
  print(i)
}

df_sim1 <- data.frame(iteration = 1:100, coef1 = estimate, coef2 = estimate1,
                      std_error1 = std_error, std_error2 = std_error1 )

p3 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef1)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef1 - std_error1, ymax = coef1 + std_error1), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression estimate of Political Change \n cw ~ pol_change + income + lag cw")

p3

p4 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef2)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef2 - std_error2, ymax = coef2 + std_error2), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression estimate of Political Change - cw ~ pol_change + income")

ggsave(p3, filename = "coef_sim_homg_effect_lag_vd1.png")

ggsave(p4, filename = "coef_sim_homg_effect.png")



##################33
## Collider bias? #
#################3##

estimate <- numeric()
estimate1 <- numeric()
estimate2 <- numeric()
estimate3 <- numeric()

std_error <- numeric()
std_error1 <- numeric()
std_error2 <- numeric()
std_error3 <- numeric()

for (i in 1:100) {
  n <- 1000
  time <- 10
  # init variables
  
  cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
  incomepc0 <- 1000*exp(rnorm(n)) # log normal
  democracy0 <- rnorm(n)
  u0 <- rnorm(n)
  
  # creates vector of variables and initialize pchange0
  pchange0 <- rbinom(n, 1, p=invlogit(-.8*log(incompe0) + rnorm(n, 0, 10))) 
  incomepc <- numeric()
  cw <- numeric()
  pchange <- numeric()
  democracy <- numeric()
  u0 <- numeric()
  
  # period 1 variables
  
  incomepc <- append(incomepc0, incompe0*runif(n, .95, 1.1) - .2*cw0 )
  pchange <- append(pchange0, rbinom(n, 1, p=invlogit(-.8*log(incomepc[1001:2000]) + 10*pchange0 + rnorm(n, 0, 8))))
  cw <- append(cw0, 3 + cw0 + 5*pchange0 - log(incomepc0) + .3*democracy0 + rnorm(n, 0, 2))
  u <- append(u0, pchange + rnorm(n))
  democracy <- append(democracy0, u[1001:2000] - cw[1001:2000] + rnorm(n))
  # 5 + cw0[1] + 5*pchange0[1] - .5*log(incomepc0[1]) + rnorm(1, 0, 3)
  # 5 + cw0[2] + 5*pchange0[2] - .5*log(incomepc0[2]) + rnorm(1, 0, 3)
  
  lag_index <- 1:n
  for ( k in 1:time) {
    incomepc <- append(incomepc,  incomepc[lag_index + n*k]*runif(n, .95, 1.1) - .2*cw[lag_index + n*k])
    pchange <- append(pchange, rbinom(n, 1, p=invlogit(-.8*log(incomepc[lag_index + n*(k+1)]) +
                                                         10*pchange[lag_index + n*k] + rnorm(n, 0, 8))))
    cw <- append(cw, 3 + cw[lag_index + n*k] + 5*pchange[lag_index + n*k] - log(incomepc[lag_index + n*k]) +
                   rnorm(n, 0, 2))
    u <- append(u, pchange[lag_index + n*k] + rnorm(n))
    democracy <- append(democracy, u[lag_index + n*(k+1)]  - cw[lag_index + n*(k+1)]  + rnorm(n))
  }
  
  df <- data.frame(cw = cw, pchange = pchange, 
                   incomepc = incomepc, u = u, democracy = democracy,
                   period = rep(1:(time+2), each= n), id = rep(1:n, time+2)) %>%
    mutate(log_income =  log(incomepc)) %>%
    group_by(id) %>%
    mutate(lag_cw = dplyr::lag(cw, order_by=period),
           lag_pchange = dplyr::lag(pchange, order_by=period),
           lag_log_income = dplyr::lag(log_income, order_by = period),
           lag_u = dplyr::lag(u, order_by=period),
           lag_democracy = dplyr::lag(democracy, order_by=period),) %>%
    dplyr::filter(period != 1)
  
  
  reg <- feols(cw ~ lag_pchange + lag_log_income + lag_cw + democracy, data = df)
  summary(reg)
  
  reg1 <- feols(cw ~ lag_pchange + lag_log_income + lag_cw , data = df)
  summary(reg1)
  
  reg_fe <- feols(cw ~ lag_pchange + lag_log_income + lag_cw  | id  + period, data = df)
  summary(reg_fe)
  
  reg_fe1 <- feols(cw ~ lag_pchange + lag_log_income  | id + period , data = df)
  summary(reg_fe1)
  
  
  estimate[i] <- coef(reg)[2]
  estimate1[i] <- coef(reg1)[2]
  estimate2[i] <- coef(reg_fe)[2]
  estimate3[i] <- coef(reg_fe1)[2]
  
  std_error[i] <- se(reg)[2]
  std_error1[i] <- se(reg1)[2]
  std_error2[i] <- se(reg_fe)[2]
  std_error3[i] <- se(reg_fe1)[2]
  print(i)
}

df_sim1 <- data.frame(iteration = 1:100, coef1 = estimate, coef2 = estimate1,
                      std_error1 = std_error, std_error2 = std_error1 )

p5 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef1)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef1 - std_error1, ymax = coef1 + std_error1), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Collider bias \n cw ~ pol_change + income + lag cw + democracy_level")

p5

p6 <- df_sim1 %>%
  ggplot(aes(x = iteration, y = coef2)) +
  geom_point() + geom_smooth(method="lm") +
  geom_errorbar(aes(ymin = coef2 - std_error2, ymax = coef2 + std_error2), width = 0.2) +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("No collider bias \n cw ~ pol_change + income + lag cw")

p6

ggsave(p5, filename = "collider_bias.png")

ggsave(p6, filename = "cno_collider_bias.png")
