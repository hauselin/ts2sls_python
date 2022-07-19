# library(broom)
library(dplyr)

reg <- function(X, y) {
    design_matrix <- model.matrix(~., X)
    mod <- lm(y ~ 0 + design_matrix)
    beta_hr <- coef(mod)
    names(beta_hr) <- c("(Intercept)", names(X))
    return(beta_hr)
}

ivregress <- function() {
    return(NULL)
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
    
    results <- ivregress(X, Z, y)  # TODO stopped here
    
    
}
