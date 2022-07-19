# library(broom)
library(dplyr)

reg <- function(X, y) {
    X <- as.data.frame(X)
    X$const <- NULL  # remove intercept because model.matrix function below includes it
    design_matrix <- model.matrix(~., X)
    mod <- lm(y ~ 0 + design_matrix)
    beta_hr <- coef(mod)
    names(beta_hr) <- c("(Intercept)", names(X))
    return(beta_hr)
}

ivregress <- function(X, Z, y, verbose = FALSE) {
    X <- as.matrix(X)
    Z <- as.matrix(Z)
    tmp1 <- Z %*% solve(t(Z) %*% Z)
    
    if (verbose) print(dim(tmp1))
    
    X_hat <- tmp1 %*% (t(Z) %*% X)
    beta_2sls <- reg(X_hat, y)
    
    if (verbose) print(beta_2sls)
    
    Pz <- tmp1 %*% t(Z)
    
    if (verbose) print(dim(Pz))
    
    eps <- y - (X %*% beta_2sls)
    sigma_2 <- (t(eps) %*% eps) / dim(X)[1]
    
    if (verbose) print(sigma_2)
    
    Var_beta_2sls <- sigma_2[1] * solve((t(X) %*% Pz) %*% X)
    
    if (verbose) print(sqrt(diag(Var_beta_2sls)))
    
    return(list(beta_2sls = beta_2sls, Var_beta_2sls = Var_beta_2sls))
    
}


ivregress_2sls <- function(df, y_var, regs, ev, inst, verbose = FALSE) {
    S <- copy(df)
    S$const <- 1
    
    # exogenous variables
    ex_vars <- setdiff(regs, ev)
    
    z_vars <- c("const", inst, ex_vars)
    x_vars <- c("const", regs)
    
    if (verbose) {
        cat("Variables in Z matrix are", z_vars, sep = " ")
        cat("\n")
        cat("Variables in X matrix are", x_vars, sep = " ")
        cat("\n")
    }
    
    Z <- select(S, all_of(z_vars))
    X <- select(S, all_of(x_vars))
    y <- select(S, all_of(y_var))[[y_var]]  # 1D vector
    
    results <- ivregress(X, Z, y) 
    
    std_err <- sqrt(diag(results$Var_beta_2sls))
    results <- data.frame(term = c("(Intercept)", x_vars[-1]),
                          coef = results$beta_2sls, 
                          stderr = std_err)
    rownames(results) <- NULL
    return(results)
        
    
}




ts2sls <- function(df1, df2, y_var, regs, ev, inst, verbose = FALSE) {
    S1 <- copy(df1)
    S2 <- copy(df2)
    
    S1$const <- 1
    S2$const <- 1
    
    # exogenous variables
    ex_vars <- setdiff(regs, ev)
    
    z_vars <- c("const", inst, ex_vars)
    x_vars <- c("const", regs)   # TODO continue here
    
    
}
