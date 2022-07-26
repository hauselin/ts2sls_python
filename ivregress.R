library(dplyr)
library(data.table)

reg <- function(X, y) {
    #' X: 2D matrix or dataframe of covariates/predictors
    #' y: 1D vector of target/outcome variable
    
    X <- as.data.frame(X)
    X$const <- NULL  # remove intercept because model.matrix function below includes it
    design_matrix <- model.matrix(~., X)
    mod <- lm(y ~ 0 + design_matrix)  # b = (X'X)^-1 (X'y)
    beta_hr <- coef(mod)
    names(beta_hr) <- c("(Intercept)", names(X))
    return(beta_hr)
}

ivregress <- function(X, Z, y, verbose = FALSE) {
    # Z: matrix of valid instrumental variables
    
    X <- as.matrix(X)
    Z <- as.matrix(Z)
    tmp1 <- Z %*% solve(t(Z) %*% Z) # Z (Z'Z)^-1
    
    if (verbose) print(dim(tmp1))
    
    X_hat <- tmp1 %*% (t(Z) %*% X)  # Z (Z'Z)^-1 (Z'X)  # stage 1 
    beta_2sls <- reg(X_hat, y)  # stage 2 (Inoue & Solon 2010 eq.3)
    
    if (verbose) print(beta_2sls)
    
    
    Pz <- tmp1 %*% t(Z)  # Z (Z'Z)^-1 (Z')
    
    if (verbose) print(dim(Pz))
    
    eps <- y - (X %*% beta_2sls)  # error
    sigma_2 <- (t(eps) %*% eps) / dim(X)[1]
    
    if (verbose) print(sigma_2)
    
    Var_beta_2sls <- sigma_2[1] * solve((t(X) %*% Pz) %*% X)
    
    if (verbose) print(sqrt(diag(Var_beta_2sls)))
    
    return(list(beta_2sls = beta_2sls, Var_beta_2sls = Var_beta_2sls))
    
}


ivregress_2sls <- function(df, y_var, regs, endo_var, instruments, verbose = FALSE) {
    S <- copy(df)
    S$const <- 1
    
    # exogenous variables
    ex_vars <- setdiff(regs, endo_var)
    
    z_vars <- c("const", instruments, ex_vars)
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

ts2sls_helper <- function(X2, X1, Z2, Z1, y1, y_z, ev_ind, verbose = FALSE){
  X2 <- as.matrix(X2) #contain k exogenous regressors and p endogenous regressors
  X1 <- as.matrix(X1)
  Z2 <- as.matrix(Z2) #contain k exogneous regressors and q instrumental variables
  Z1 <- as.matrix(Z1)
  y1 <- as.matrix(y1)
  y_z <- as.matrix(y_z)
  
  # Inoue & Solon 2010 eq.10
  tmp_z2t2_inv <- solve(t(Z2) %*% Z2)  # (Z2'Z2)^-1
  tmp_z2tx2 <- t(Z2) %*% X2  # Z2' (Z2'Z2)^-1
  X1_hat <- Z1 %*% (tmp_z2t2_inv %*% tmp_z2tx2)
  beta_t2sls <- solve(t(X1_hat) %*% X1_hat) %*% (t(X1_hat) %*% y1)  # eq.10
  
  n1 <- nrow(Z1)
  n2 <- nrow(Z2)
  
  beta_1s <- reg(Z2, y_z)
  pred_y_z <- (Z1 %*% beta_1s)
  
  pred_X1 <- X1
  pred_X1[, ev_ind] <- pred_y_z[, 1]
  
  k_p <- ncol(pred_X1)
  eps <- y1 - pred_X1 %*% beta_t2sls
  sigma_2 <- (t(eps) %*% eps) / (n1 - k_p) #what is this ??
  if (verbose) { print(sigma_2)}
  
  # hmmm Var_beta_2sls is not used anywhere later?
  Var_beta_2sls <- sigma_2[1] * solve(t(X1_hat) %*% X1_hat)
  if (verbose){
    print(Var_beta_2sls)
    print(sqrt(diag(Var_beta_2sls)))
  }
  
  k_q <- ncol(Z2)
  pred_X2 <- X2
  pred_y_z2 <- Z2 %*% beta_1s
  pred_X2[, ev_ind] <- pred_y_z2[, 1]
  eps_1s <- X2 - pred_X2
  sigma_nu <- (t(eps_1s) %*% eps_1s) / (n2 - k_q) #what is this ??
  if (verbose) { print(sigma_nu)}
  #TODO: what is sigma_f ??
  sigma_f <- sigma_2 + n1 / n2 * t(beta_t2sls) %*% ((sigma_nu) %*% beta_t2sls)
  if (verbose) {print(sigma_f)}
  Var_beta_ts2sls <- sigma_f[1] * solve(t(X1_hat) %*% X1_hat)
  
  if (verbose) {
    print(Var_beta_ts2sls)
    print(sqrt(diag(Var_beta_ts2sls)))
  }
  
  return(list(beta_t2sls = beta_t2sls, Var_beta_ts2sls = Var_beta_ts2sls))
}




ts2sls <- function(df1, df2, y_var, regs, endo_var, instruments, verbose = FALSE) {
    S1 <- copy(df1)
    S2 <- copy(df2)
    
    S1$const <- 1
    S2$const <- 1
    
    # exogenous variables
    ex_vars <- setdiff(regs, endo_var)
    
    z_vars <- c("const", instruments, ex_vars)
    x_vars <- c("const", regs) 
    
    if (verbose){
      print("Variables in Z matrix are")
      print(z_vars)
      print("Variables in X matrix are")
      print(x_vars)
    }
    
    Z2 <- select(S2, all_of(z_vars))
    Z1 <- select(S1, all_of(z_vars))
    X2 <- select(S2, all_of(x_vars))
    X1 <- select(S1, all_of(x_vars))
    y1 <- select(S1, all_of(y_var))
    y_z <- select(S2, all_of(endo_var)) 
    
    endo_var_ind <- match(endo_var, x_vars)
    
    results <- ts2sls_helper(X2, X1, Z2, Z1, y1, y_z, endo_var_ind)
    std_err <- sqrt(diag(results$Var_beta_ts2sls))
    
    results <- data.frame(term = c("(Intercept)", x_vars[-1]),
                          coef = as.numeric(results$beta_t2sls), 
                          stderr = std_err)
    rownames(results) <- NULL
    return(results)
    
}
