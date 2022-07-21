rm(list = ls())
library(data.table)
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
ev <- "mpg"
inst <- "headroom"
result <- ivregress_2sls(S, y_var, regs, ev, inst, verbose = T)
result

#%% t2sls - single instrument

y_var <- "price"
regs <- c("weight", "mpg")
ev <- "mpg"
inst <- "headroom"
result <- ts2sls(S1, S2, y_var, regs, ev, inst)
print(result)

