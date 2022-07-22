rm(list = ls())
library(data.table); library(fixest)
source("ivregress.R")

df1 <- fread("auto_data.csv")
df1[, weight2 := weight * weight]
df2 <- copy(df1)
df1$mpg <- NA

S1 <- df1
S2 <- df2

#%% iveregress - single instrument

S <- df2
y_var  <- "price"
regs <- c("weight", "mpg")
endo_var <- "mpg"
instruments <- "headroom"
result <- ivregress_2sls(S, y_var, regs, endo_var, instruments, verbose = T)
result
feols(price ~ weight | mpg ~ headroom, data = S)

#%% t2sls - single instrument

y_var <- "price"
regs <- c("weight", "mpg")
endo_var <- "mpg"
instruments <- "headroom"
result <- ts2sls(S1, S2, y_var, regs, endo_var, instruments)
print(result)

