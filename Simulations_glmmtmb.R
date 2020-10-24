# Simulation --------------------------------------------------------------
# y_i = a + (β_g)*x_i + ε_i
# with groups g and observations i
library(glmmTMB)
set.seed(1)


#### NORMAL Random intercept -----------------------------------------------

n <- 100

## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)
sd_randeff = 0.2*beta[1]

result_list = list()
for(number_groups in 2:8){
  n_groups <- number_groups
  
  number_experiments = 10000
  results = matrix(nrow = number_experiments, ncol = 8)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "se_intercept", "se_effect",
                        "stddev_randeff", "Convergence")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 
    
    ## The normal model
    sigma <- 0.5
    y <- rnorm(n, mu, sd = sigma) 
    
    fit_glmmtmb <- glmmTMB(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random
    
    results[experiment,1] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Estimate"]
    results[experiment,2] = summary(fit_glmmtmb)$coefficients$cond["x","Estimate"]
    results[experiment,3] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_glmmtmb)$coefficients$cond["x","Pr(>|z|)"]
    results[experiment,5] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Std. Error"]
    results[experiment,6] = summary(fit_glmmtmb)$coefficients$cond["x","Std. Error"]
    results[experiment,7] = attr(summary(fit_glmmtmb)$varcor$cond$group,"stddev")
    results[experiment,8] = as.integer(!fit_glmmtmb$sdr$pdHess)
    
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "Results/results_intercept_glmmtmb_normal.Rds")


#### NORMAL Random slope -----------------------------------------------


sd_randslope = 0.2*beta[2]
result_list_slope = list()
for(number_groups in 2:8){
  n_groups <- number_groups
  
  number_experiments = 10000
  results = matrix(nrow = number_experiments, ncol = 8)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "se_intercept", "se_effect",
                        "stddev_randeff", "Convergence")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randslope <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)
    # maybe do some correlation between random slope and fixed intercept 
    #MASS::mvrnorm()
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]])) 
    
    ## The normal model
    sigma <- 0.5
    y <- rnorm(n, mu, sd = sigma) 
    
    fit_glmmtmb <- glmmTMB(y ~ x  + (0 + x | group)) # only beta[c(1,2)] are also random
    
    results[experiment,1] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Estimate"]
    results[experiment,2] = summary(fit_glmmtmb)$coefficients$cond["x","Estimate"]
    results[experiment,3] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_glmmtmb)$coefficients$cond["x","Pr(>|z|)"]
    results[experiment,5] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Std. Error"]
    results[experiment,6] = summary(fit_glmmtmb)$coefficients$cond["x","Std. Error"]
    
    results[experiment,7] = attr(summary(fit_glmmtmb)$varcor$cond$group,"stddev")
    results[experiment,8] = as.integer(!fit_glmmtmb$sdr$pdHess)
    
  }
  
  result_list_slope[[number_groups-1]] = results
}

saveRDS(result_list_slope, file = "Results/results_slope_glmmtmb_normal.Rds")






#### Binomial Random intercept -----------------------------------------------

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}

n <- 1000L

## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(2,0.3)
sd_randeff = 0.2*beta[1]

result_list = list()
for(number_groups in 2:8){
  n_groups <- number_groups
  
  number_experiments = 10000
  results = matrix(nrow = number_experiments, ncol = 8)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "se_intercept", "se_effect",
                        "stddev_randeff", "Singularity")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 
    
    ## The binomial model
    y <- rbinom(n, 1, inv.logit(mu))
    
    fit_glmmtmb <- glmmTMB(y ~ x  + (1 | group), family = binomial)
    
    results[experiment,1] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Estimate"]
    results[experiment,2] = summary(fit_glmmtmb)$coefficients$cond["x","Estimate"]
    results[experiment,3] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_glmmtmb)$coefficients$cond["x","Pr(>|z|)"]
    results[experiment,5] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Std. Error"]
    results[experiment,6] = summary(fit_glmmtmb)$coefficients$cond["x","Std. Error"]
    
    results[experiment,7] = attr(summary(fit_glmmtmb)$varcor$cond$group,"stddev")
    results[experiment,8] = as.integer(!fit_glmmtmb$sdr$pdHess)
    
  }
  
  result_list[[number_groups-1]] = results
}
saveRDS(result_list, file = "Results/results_intercept_glmmtmb_binomial.Rds")


#### Binomial Random slope -----------------------------------------------

n <- 1000L

## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
sd_randslope = 0.8*beta[2]

result_list_slope = list()
for(number_groups in 2:5){
  n_groups <- number_groups
  
  number_experiments = 10000
  results = matrix(nrow = number_experiments, ncol =8)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "se_intercept", "se_effect",
                        "stddev_randeff", "Singularity")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randslope <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]]))  
    
    ## The binomial model
    y <- rbinom(n, 1, inv.logit(mu))
    
    fit_glmmtmb <- glmmTMB(y ~ x  + (0 + x | group), family = binomial)
    
    results[experiment,1] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Estimate"]
    results[experiment,2] = summary(fit_glmmtmb)$coefficients$cond["x","Estimate"]
    results[experiment,3] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_glmmtmb)$coefficients$cond["x","Pr(>|z|)"]
    results[experiment,5] = summary(fit_glmmtmb)$coefficients$cond["(Intercept)","Std. Error"]
    results[experiment,6] = summary(fit_glmmtmb)$coefficients$cond["x","Std. Error"]
    
    results[experiment,7] = attr(summary(fit_glmmtmb)$varcor$cond$group,"stddev")
    results[experiment,8] = as.integer(!fit_glmmtmb$sdr$pdHess)
    
  }
  
  result_list_slope[[number_groups-1]] = results
}
saveRDS(result_list_slope, file = "Results/results_slope_glmmtmb_binomial.Rds")
