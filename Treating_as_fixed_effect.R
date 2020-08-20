### Same setup, random effect is fitted as fixed effect 
set.seed(1)
n <- 100

x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)
sd_randeff = 0.2*beta[1]
result_list = list()
for(number_groups in 2:10){
  n_groups <- number_groups
  number_experiments = 1000
  results = matrix(nrow = number_experiments, ncol =4)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 
    
    ## The normal model
    sigma <- 0.5
    y <- rnorm(n, mu, sd = sigma) 
    
    fit_lm <- lm(y ~ x  +  group) 
    
    results[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] 
    + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"])  
    results[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
    results[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
    results[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
    
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "Results/results_intercept_fixed_effect.Rds")

set.seed(1)
n <- 100

x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)
sd_randslope = 0.2*beta[2]
result_list = list()
for(number_groups in 2:10){
  n_groups <- number_groups
  number_experiments = 1000
  results = matrix(nrow = number_experiments, ncol =4)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randintercep <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randintercep[g[i]])) 
    
    ## The normal model
    sigma <- 0.5
    y <- rnorm(n, mu, sd = sigma) 
    
    fit_lm <- lm(y ~ x*group) 
    
    results[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] 
    results[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"] +
      + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"]) 
    results[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
    results[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
    
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "Results/results_slope_fixed_effect.Rds")


n <- 1000L

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}
## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(2,0.3)
sd_randeff = 0.2*beta[1]
result_list = list()
for(number_groups in 2:10){
  n_groups <- number_groups
  
  number_experiments = 1000
  results = matrix(nrow = number_experiments, ncol =5)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "stddev_randeff")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 
    
    ## The binomial model
    y <- rbinom(n, 1, inv.logit(mu))
    
    fit_lmm <- glm(y ~ x  +  group, family = binomial)#,control = glmerControl(optimizer = "Nelder_Mead")) # only beta[c(1,2)] are also random
    
    results[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"] 
    + (1/n_groups)*sum(summary(fit_lmm)$coefficients[-(1:2),"Estimate"])  
    results[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
    results[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
    
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "Results/results_glm_fixed_effect.Rds")


n <- 1000L

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}
## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 

beta <- c(2,0.3)
sd_randslope = 0.2*beta[2]
result_list = list()
for(number_groups in 2:10){
  n_groups <- number_groups
  
  number_experiments = 1000
  results = matrix(nrow = number_experiments, ncol =5)
  colnames(results) = c("estimate_intercept","estimate_effect", 
                        "p_value_intercept", "p_value_effect",
                        "stddev_randeff")
  
  for(experiment in 1:number_experiments){
    
    g <- sort(sample.int(n_groups, size = n, replace = T))
    group <-  as.factor(g)
    randslope <- rnorm(n_groups, mean = beta[2], sd = sd_randslope)
    
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]])) 
    
    ## The binomial model
    y <- rbinom(n, 1, inv.logit(mu))
    
    fit_lmm <- glm(y ~ x*group, family = binomial)#,control = glmerControl(optimizer = "Nelder_Mead")) # only beta[c(1,2)] are also random
    
    results[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"] 
    results[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]  + 
      (1/n_groups)*sum(summary(fit_lmm)$coefficients[-(1:2),"Estimate"])  
    results[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
    results[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
    
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "Results/results_glm_fixed_slope.Rds")


