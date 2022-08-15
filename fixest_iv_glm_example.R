# https://github.com/lrberge/fixest/issues/176

# Function to get the estimates
ivpois_est = function(fml, data){
    # 1st + 2nd stage OLS
    est_ols = feols(fml, data)
    
    # Getting the residuals
    resid_1st_stage = resid(summary(est_ols, stage = 1))
    
    # 2nd stage poisson, we add the 1st stage residuals as regressors
    y = model.matrix(est_ols, type = "lhs")
    RHS = model.matrix(est_ols, type = c("iv.endo", "iv.exo"))
    RHS_control_fun = cbind(RHS, resid_1st_stage)
    
    # FEs, if there // note that vars with varying slopes won't work
    FE = if(!is.null(est_ols$fixef_vars)) model.matrix(est_ols, type = "fixef") else NULL
    
    # 2nd stage
    feglm.fit(y, RHS_control_fun, FE, family = "poisson")
}

# Function to get the VCOV
ivpois_vcov = function(fml, data, nrep){
    # This is just to give an example, there are many BS schemes possible
    
    n = nrow(data)
    all_coef = vector("list", nrep)
    
    for(i in 1:nrep){
        all_coef[[i]] = coef(ivpois_est(fml, data[sample(n, n/2), ]))
    }
    
    all_coef_mat = do.call("rbind", all_coef)
    
    var(all_coef_mat)
}

# Function to get both the VCOV and the estimates
ivpois = function(fml, data, nrep = 200){
    
    res = ivpois_est(fml, data)
    
    vcov = ivpois_vcov(fml, data, nrep)
    
    summary(res, .vcov = vcov)
}

# Example
base = iris
names(base) = c("y", "x1", "endo", "inst", "fe")

est_noIV = fepois(y ~ endo + x1 | fe, base)
est_IV = ivpois(y ~ x1 | fe | endo ~ inst, base)
est_IV_noboot = ivpois_est(y ~ x1 | fe | endo ~ inst, base)

etable(est_noIV, est_IV, est_IV_noboot)
#>                           est_noIV             est_IV     est_IV_noboot
#> Dependent Var.:                  y                  y                 y
#>                                                                        
#> endo            0.1221*** (0.0150)   0.0701. (0.0409) 0.0701** (0.0272)
#> x1                0.0778* (0.0339) 0.1033*** (0.0251) 0.1033** (0.0314)
#> resid_1st_stage                       0.0599 (0.0406) 0.0599** (0.0222)
#> Fixed-Effects:  ------------------ ------------------ -----------------
#> fe                             Yes                Yes               Yes
#> _______________ __________________ __________________ _________________
#> S.E. type                   by: fe             Custom            by: fe
#> Observations                   150                150               150
#> Squared Cor.               0.86314            0.86715           0.86715
#> Pseudo R2                  0.02676            0.02687           0.02687
#> BIC                         570.76             575.71            575.71
