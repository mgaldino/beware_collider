## Simularions for dynamics models and foreign collider bias

# loading packages
library(ggdag)
library(arm)
library(tidyr)
library(tidyverse)
library(fixest)
library(plm)

# fristly, let's plot the DAG that will be simulated
lag_treatment <- ggdag::dagify(cw2 ~ cw1 + pchange1 + incomepc1,
                               cw1 ~ cw0 + pchange0 + incomepc0,
                               pchange1 ~ pchange0 + incomepc1,
                               incomepc1 ~ incomepc0,
                               pchange0 ~ incomepc0
)

ggdag::ggdag(lag_treatment, text_col = "red", label_col = "white")

## Monte Carlo Simulation

##############3########
### Dynamic panel model

# variables where we will save regession coef estimates
estimate <- numeric()
estimate1 <- numeric()
estimate2 <- numeric()
estimate3 <- numeric()

# variables where we ill save regressio ncoef se 
std_error <- numeric()
std_error1 <- numeric()
std_error2 <- numeric()
std_error3 <- numeric()

set.seed(1234)

# loop for MC simulation
# k iterations
k <- 1000
for (i in 1:k) {
  n <- 1000
  time <- 10
# init variables

cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
incomepc0 <- 1000*exp(rnorm(n)) + 100 # log normal

# creates vector of variables and initialize pchange0
pchange0<- rbinom(n, 1, p=invlogit(-.8*log(incomepc0) + rnorm(n, 0, 10))) 
incomepc <- numeric()
cw <- numeric()
pchange <- numeric()
# period 1 variables
incomepc <- append(incomepc0, incomepc0*runif(n, .95, 1.1))
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
if( i %% 10 == 0) print(i)
}

# creating data frame to store variables of MC simulation
df_sim1 <- data.frame(iteration = 1:k, coef1 = estimate, coef2 = estimate1,
                      std_error1 = std_error, std_error2 = std_error1 )

# Does 95% percent of IC contain the true value?
df_sim1 %>%
  mutate(lower = coef1 - 2*std_error1,
         upper = coef1 + 2*std_error1) %>%
  summarise(int_95 = sum(abs(lower < 5 & upper > 5))/n())

# Yep  95,2%

# plotting ppaer graphics

set.seed(34)
# selecting 100 obs of k simulations
# otherwise the graph is too polluted
df_plot <- df_sim1 %>%
  sample_n(size = 100)

# first plot - with lagged DV
p3 <- df_plot %>%
  ggplot(aes(x = 1:100, y = coef1)) +
  geom_hline(yintercept = 5, colour = "red") +
  geom_hline(aes(yintercept = mean(coef1)), colour = "blue", linetype='dotted') +
  geom_errorbar(aes(ymin = coef1 - 1.96*std_error1, ymax = coef1 + 1.96*std_error1), width = 0.2) +
  geom_point( size=3, shape=21, fill="white") +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ylim(4.75, 5.25) +
  ggtitle("Regression estimate of Political Change \n cw ~ pol_change_lag + income_lag + cw_lag")

p3

ggsave(p3, filename = "coef_sim_homg_effect_lag_vd1.png", scale = .8)

  # second plot, withou lagged DV
p4 <-  df_plot %>%
  ggplot(aes(x = 1:100, y = coef2)) +
  geom_point() +
  geom_hline(yintercept = 5, colour = "red", linetype='dotted') +
  geom_hline(aes(yintercept = mean(coef2)), colour = "blue") + 
  geom_errorbar(aes(ymin = coef2 - 1.96*std_error2, ymax = coef2 + 1.96*std_error2), width = 0.2) +
  geom_point( size=3, shape=21, fill="white") + 
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("Regression estimate of Political Change \n cw ~ pol_change_lag + income_lag")

p4


ggsave(p4, filename = "coef_sim_homg_effect.png", scale= .8)


##################33
## Fo]reign Collider bias #
#################3##

estimate <- numeric()
estimate1 <- numeric()
estimate2 <- numeric()
estimate3 <- numeric()

std_error <- numeric()
std_error1 <- numeric()
std_error2 <- numeric()
std_error3 <- numeric()

set.seed(1234)
for (i in 1:k) {
  n <- 1000
  time <- 10
  # init variables
  
  cw0 <- -2 + rnorm(n, 0, 3) # 30% tem civil war no tempo t0 
  incomepc0 <- 1000*exp(rnorm(n)) + 100 # log normal
  democracy0 <- rnorm(n)
  u0 <- rnorm(n)
  
  # creates vector of variables and initialize pchange0
  pchange0 <- rbinom(n, 1, p=invlogit(-.8*log(incomepc0) + rnorm(n, 0, 10))) 
  incomepc <- numeric()
  cw <- numeric()
  pchange <- numeric()
  democracy <- numeric()
  u0 <- numeric()
  
  # period 1 variables
  
  incomepc <- append(incomepc0, incomepc0*runif(n, .95, 1.1) - .2*cw0 )
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
  
  if ( i %% 10 == 0) print(i)
}

df_sim1 <- data.frame(iteration = 1:k, coef1 = estimate, coef2 = estimate1,
                      std_error1 = std_error, std_error2 = std_error1 )

df_sim1 %>%
  mutate(lower = coef2 - 2*std_error2,
         upper = coef2 + 2*std_error2) %>%
  summarise(int_95 = sum(abs(lower < 5 & upper > 5))/n())

# 95.7%

set.seed(34)
df_plot <- df_sim1 %>%
  sample_n(size = 100)

p5 <- df_plot %>%
  ggplot(aes(x = 1:100, y = coef1)) +
  geom_hline(aes(yintercept = mean(coef1)), colour = "blue", linetype='dotted') +
  geom_errorbar(aes(ymin = coef1 - 1.96*std_error1, ymax = coef1 + 1.96*std_error1), width = 0.2) +
  geom_point( size=2, shape=21, fill="white") +
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ylim(2, 3) +
  ggtitle("Regression estimate - Collider bias \n cw ~ pol_change_lag + income_lag + cw_lag +  \n democracy_level")

p5
ggsave(p5, filename = "collider_bias.png", scale = .7)

p6 <-  df_plot %>%
  ggplot(aes(x = 1:100, y = coef2)) +
  geom_point() +
  geom_hline(yintercept = 5, colour = "red", linetype='dotted') +
  geom_hline(aes(yintercept = mean(coef2)), colour = "blue") + 
  geom_errorbar(aes(ymin = coef2 - 1.96*std_error2, ymax = coef2 + 1.96*std_error2), width = 0.2) +
  geom_point( size=3, shape=21, fill="white") + 
  labs(x = "Iteration", y = "Coefficient Estimate") + 
  theme_bw() +
  ggtitle("No collider bias \n cw ~ pol_change_lag + income_lag + cw_lag")

p6

ggsave(p6, filename = "no_collider_bias.png",  scale = .8)
