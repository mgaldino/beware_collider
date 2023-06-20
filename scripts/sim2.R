
library(MASS)
library(tidyverse)
n <- 10000
sd_x <- 3
sd_z <- 4
cor_xz <- -.6
sigma_mat <- matrix(c(sd_x^2, sd_x*sd_z*cor_xz , sd_x*sd_z*cor_xz , sd_z^2), 2, ,2)

x_z <- mvrnorm(n = n, rep(0, 2), sigma_mat)

x <-  x_z[,1]
y1 <- rnorm(n)
z <- x_z[,2] -2*y1
y2 <- .5*y1 + rnorm(n)

df <- data.frame(x = x, y1 = y1, y2 = y2, z = z)
reg <- lm(y2 ~ x + z)
summary(reg)

reg1 <- lm(y2 ~ x)
summary(reg1)

with(df, cor(x,z))

##
# fixed variance and correlation between x and z across time and space
# no correlation of x over time cor(x_i,1; x_i,2) = 0.
# no correlation of z over time
n <- 1000
time <- 201
my_list <- list()
y <- rnorm(n)

for ( i in 1:time) {
  mat <- mvrnorm(n = n, rep(0, 2), sigma_mat)
  y <- rnorm(n) + .3*y
  my_list[[i]] <-  data.frame(x = mat[,1], z = mat[,2], y=y)
}
df1 <- bind_rows(my_list) %>%
  mutate(id = rep(1:n, each=time), time = rep(1:time, n)) %>%
  group_by(id) %>%
  mutate(y_lag = lag(y),
         z1 = z - .6*y_lag,
         time = time -1) %>%
  filter(!is.na(z1))

reg <- lm(y ~x + z1, data=df1)
summary(reg)

cor(df1$y, df1$z1)
