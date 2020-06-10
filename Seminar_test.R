# Simulation --------------------------------------------------------------
# y_i = a + (β_g)*x_i + ε_i
# with groups g and observations i
library(lme4)
library(lmerTest)
set.seed(1)
n <- 100

## The data generating process ### 
x <- runif(n, -1, 1)
X <- matrix(c(rep(1, n), x), nrow = n) # 
beta <- c(66, 3)
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

  ## The normal model
  sigma <- 0.5
  y <- rnorm(n, mu, sd = sigma) 

  fit_lmm <- lmer(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random

  results[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
  results[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
  results[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
  results[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
  results[experiment,5] = attr(summary(fit_lmm)$varcor$group, "stddev")
  
  }
  
  result_list[[number_groups-1]] = results
}

saveRDS(result_list, file = "results_intercept.Rds")

par(mfrow = c(2,3))
for(i in 1:3){
  hist(result_list[[i]][,3], 
       main = paste("number of groups:", i+1),
       xlab = "estimated p-value",
       breaks = 100  )
  abline(v = 0.05, col = "red")
}

for(i in 1:3){
  hist(result_list[[i]][,5], 
       main = "",
       xlab = "estimated standard deviation of random intercept",
       breaks = 100  )
  abline(v = 0.2*66, col = "red")
}


######### random slope #########

sd_randslope = 0.2*beta[2]
result_list_slope = list()
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
    # maybe do some correlation between random slope and fixed intercept 
    #MASS::mvrnorm()
    mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta[1],randslope[g[i]])) 
    
    ## The normal model
    sigma <- 0.5
    y <- rnorm(n, mu, sd = sigma) 
    
    fit_lmm <- lmer(y ~ x  + (0 + x | group)) # only beta[c(1,2)] are also random
    
    results[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
    results[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
    results[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
    results[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
    results[experiment,5] = attr(summary(fit_lmm)$varcor$group, "stddev")
    
  }
  
  result_list_slope[[number_groups-1]] = results
}

saveRDS(result_list_slope, file = "results_slope.Rds")

par(mfrow = c(2,3))
for(i in 1:3){
  hist(result_list_slope[[i]][,4], 
       main = paste("number of groups:", i+1),
       xlab = "p-value of effect", 
       breaks = 30)
  abline(v = 0.05, col = "red")
}


for(i in 1:3){
  hist(result_list_slope[[i]][,5], 
       xlab = "estimated standard deviation of random slope", 
       main = "",
       breaks = 30)
  abline(v = 0.2*3, col = "red")
}



#### sensitivity analysis ####
library(sensitivity)
#parameters : 
#1. sd_dev of randomeff
#2. number of groups
#3. sd of statitsical error
#4. number of data_points

## this is a quick fix for the number of groups
## there should be some method to deal with discrete variables 
## but i am not aware of it 


p_value_effectssize <- function(param){
  
  values = matrix(nrow = nrow(param), ncol = 2)
  for(j in 1:nrow(param)){
  
  n <- round(param[j,4],0)
  x <- runif(n, -1, 1)
  X <- matrix(c(rep(1, n), x), nrow = n) # 
  beta <- c(66, 3)
  sd_randeff = param[j,1]*beta[1]
  n_groups <- round(param[j,2],0) 
  g <- sort(sample.int(n_groups, size = n, replace = T))
  group <-  as.factor(g)
  randintercep <- rnorm(n_groups, mean = beta[1], sd = sd_randeff)
  
  mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta[2])) 
  
  sigma <- param[j,3]
  y <- rnorm(n, mu, sd = sigma) 
  fit_lmm <- lmer(y ~ x  + (1 | group))
  
  values[j,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
  values[j,2] = summary(fit_lmm)$coefficients["x","Estimate"]
  }
  return(values[,1])
}

par(mfrow = c(1,1))

lower = c(0.1,2,0.1,100)
upper = c(0.3,10,10,1000)

morris_out <- morris(model = p_value_effectssize,factors = 4,
                     design = list(levels = 9, grid.jump = 4),
                     binf = lower, bsup = upper,r = 100)


plot(morris_out)  
    