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
