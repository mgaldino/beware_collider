## y_it ~N(mu, sigma2)
# mu = a_i + a_t + beta*x_it
# a_i ~ N(mu_i, sigma2)
# a_t ~ N(mu_t, sigma2)

library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)
library(fixest)

n <- 200
my_t <- 40
beta <- 2
beta2 <--2

independent_x <- rnorm(n)


generate_ar <- function(x0, t=20) {
  my_ts <- x0

  for (i in 1:t) {
    epsilon <- rnorm(1)
    my_ts[i+1] <- .5*my_ts[i] + epsilon
  }
  
  return(my_ts)
}


my_list <- x %>%
  map(function(x) generate_ar(x0 = x, t = my_t-1))

df_sim <- do.call(rbind.data.frame, my_list)
names(df_sim) <- paste("y", 1:my_t, sep="0")

df_sim <- df_sim %>%
  mutate(id = 1:n) %>%
  pivot_longer(cols=!id, names_to = "time", values_to = "x", names_prefix ="y") %>%
  mutate(time = as.numeric(time))

df_sim <- df_sim %>%
  mutate(a_t = rep(rnorm(my_t), n)) %>%
  arrange(time) %>%
  mutate(a_i = rep(rnorm(n), my_t),
         mu = a_t + a_i + beta*x) %>%
  group_by(id) %>%
  mutate(x_lag = lag(x)) %>%
  ungroup()   %>%
  arrange(id)

df_sample <- df_sim %>%
  dplyr::filter(id %in% 1:3) %>%
  filter(time %in% 1:3)

# by_id
df_teste$y[1] <- rnorm(1, mu, sd=3 )
df_teste$x_lag[2] <- df_teste$x_lag[2] + beta2*df_teste$y[1]
df_teste$$x[1] <- lead(df_teste$x_lag[2])
df_sample$y[2] <- rnorm(1, beta*x[2] + a_i + a_t, sd=3)
df_sample$x_lag[3] <- x_lag[3] + beta2*df_sample$y[2]
df_sample$x[2] <- lead(x_lag[3])
df_sample$y[3] <- rnorm(1, beta*x[3] + a_i + a_t, sd=3)

# generalizando: by_id
df_sample$y[1] <- rnorm(1, mu, sd=3 )

# itera i =1 até n

df_sample$x_lag[i+1] <- x_lag[i+1] + beta2*df_sample$y[i]
df_sample$x[i] <- lead(x_lag[i+1])
df_sample$y[i] <- rnorm(1, beta*x[i+1] + a_i + a_t, sd=3)


# my_collider_function <- function(df, x_lag, y, x, n) {
#   for ( i in 1:n) {
#     df_sample$x_lag[i+1] <- x_lag[i+1] + beta2*df_sample$y[i]
#     df_sample$x[i] <- lead(x_lag[i+1])
#     df_sample$y[i] <- rnorm(1, beta*x[i+1] + a_i + a_t, sd=3)
#   }
# }

my_collider_function <- function(x_lag, y, x, n, beta2, a_i, a_t) {
  for ( i in 1:n) {
    x_lag[i+1] <- x_lag[i+1] + beta2*y[i]
    x[i] <- lead(x_lag[i+1])
    y[i] <- rnorm(1, beta*x[i+1] + a_i + a_t, sd=3)
  }
  return(y)
}


df_sim <- df_sim  %>%
  mutate(y = rnorm(n*my_t, mean = mu , sd = 3)) # %>%
  # arrange(id) %>%
  # filter(!is.na(x_t1))

# 
df_teste <- df_sim %>%
  filter(id == 1)

df_teste %>%
  mutate(y_new = my_collider_function(n = n(), x_lag = x_lag, y = y, x = x, beta2 = beta2, a_i= a_i, a_t = a_t))

df_teste <- df_sim %>%
  group_split(id) %>%
  purrr::map_dfr(my_collider_function(x_lag = x_lag, y = y, x = x, n = n, beta2 = beta2))

reg_pooled_no_collider <- feols(fml  = y ~x,  se = "iid", data = df_sim)
reg_pooled_no_collider


reg_pooled_collider <- feols(fml  = y_collider ~x, se = "iid", data = df_sim)
reg_pooled_collider


reg_unpooled_no_collider <- feols(y ~ x + x_t1 | id + time, data = df_sim, se = "iid")
reg_unpooled_no_collider 

reg_unpooled_collider_lag <- feols(y_collider ~ x + x_t1 | id + time, data = df_sim, se = "iid")
reg_unpooled_collider_lag 

reg_unpooled_collider <- feols(y_collider ~ x | id + time, data = df_sim, se = "iid")
reg_unpooled_collider 

coef(reg_unpooled_collider)

# df_sim <- df_sim %>%
#   +   mutate(a_t = rep(rnorm(my_t), n)) %>%
#   +   arrange(time) %>%
#   +   mutate(a_i = rep(rnorm(n), my_t),
#              +          mu = a_t + a_i + beta*x) %>%
#   +   group_by(id) %>%
#   +   mutate(x_lag = lag(x)) %>%
#   +   ungroup()   %>%
#   +   arrange(id)
