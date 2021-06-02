grand_mean = function(fit, mountain, beta, weighted = TRUE, z_statistic = FALSE, mode = "old") {
  
  ind = (mountain+1):(2*mountain)
  covariance = vcov(fit)[ind,ind]
  effect = summary(fit)$coefficients[ind, 1]
  
  if(!weighted) weights = rep(1/mountain, mountain)
  else weights = (1/apply(covariance, 1,sum))/ (sum(1/apply(covariance,1,sum)))
  if(mode == "old") weights = (apply(covariance, 1,sum))/ (sum(covariance))
  effect_sizes = effect
  eff = weighted.mean(effect_sizes, weights)
  var1 = 0
  for(i in 1:mountain){
    for(j in 1:mountain){
      var1 = var1 + covariance[i,j]*weights[i]*weights[j]  
    }
  }
  var2 = sum((weights/(mountain-1))*(effect_sizes-eff)^2)  
  se = sqrt(var1 + var2)
  
  if(z_statistic) {
    p_value = 2*pnorm(abs(eff/se), lower.tail = FALSE)
    confs = cbind( eff -1.96*se, eff + 1.96*se)
  } else {
    p_value = 2*pt(abs(eff/se),df=mountain-1, lower.tail = FALSE)
    bound = qt(0.975, df = mountain-1)
    confs = cbind( eff - bound*se, eff + bound*se) 
  }
  # c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
  return(c(eff, p_value, se, as.integer(beta > confs[1,1] & beta < confs[1,2])))
}



mountains = 100
balanced = runif(120, 0, 0.9)

parameter = data.frame( mountain = mountains,  balanced = balanced)
colnames(parameter) = c("mountains", "balanced")
parameter$sd = NA
parameter = data.frame(parameter)
parameter$min = NA
parameter$max = NA
parameter$TypeIold = NA
parameter$TypeInew = NA
parameter$TypeMeta = NA

cl = snow::makeCluster(30L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest)})
snow::clusterExport(cl, list("grand_mean", "parameter"), envir = environment())

results_list = vector("list", 5)
res_sd = vector("list", 3)
for(sd in 1:3) {
  sd_re = c(0.1, 0.5, 2.0)
  snow::clusterExport(cl, list( "sd_re"), envir = environment())
  for(m in 1:5) {
    parameter$mountains = c(3, 5, 10, 50, 200)[m]
    snow::clusterExport(cl, list("grand_mean", "parameter"), envir = environment())
  
    tmp = 
      snow::parLapply(cl, 1:nrow(parameter), function(i) {
        
        n_each <- 25
        n_groups = number_groups = parameter[i,]$mountains
        n = (number_groups)*n_each
        balanced = parameter[i,]$balanced
        range = (1 - balanced)/2
        continue = TRUE
        while(continue) {
          prob = runif(n_groups, range, 1-range)
          g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = prob)
          if(min(table(g)) > 2) continue = FALSE
        } # Grouping variable (mountain range)
        group = as.factor(g)
        min_max = c(min(table(g)), max(table(g)))
        parameter[i,]$min = min_max[1]
        parameter[i,]$max = min_max[2]
        
        nonSing2 = 1
        abort2 = 1
        res = matrix(NA, 2000, 3)
        while(nonSing2 < 2001){
          abort2 = abort2 + 1
          if( (abort2 == 20000)) break
        
          x <- runif(n, -1, 1) # Temperature
          X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
          
          sd_randeff = sd_re # sd for random effects
          
          beta = 0.0   # Temperature effect meaning when the temperature increases 1 degree the respective variable also does 
          beta0 = 10.0   # Intercept: the mean height of a reporductive plant  
          
          randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)  # random intercept
          randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)      # random slope
          
          mu <- sapply(1:n, FUN = function(j) X[j,] %*% c(randintercep[g[j]],randslope[g[j]]))
          
          sigma <- 0.5
          y <- rnorm(n, mu, sd = sigma)
          
          # lme4 - REML
          if(nonSing2 < 2001) {
            try({
              fit_lmm <- lmer(y ~ x  + (1|group) + (0+x | group), REML = TRUE) 
            }, silent = TRUE)
            
            
            # linear model w mountain range as grouping variable
            if(is.null(fit_lmm@optinfo$conv$lme4$messages)) {
              try({
                fit_lm = lm(y ~ 0+x*group-x)
                res[nonSing2, 1] = grand_mean(fit_lm, n_groups, beta, mode = "old")[2]
                res[nonSing2, 2] = grand_mean(fit_lm, n_groups, beta, mode = "new")[2]
                ind = (n_groups+1):(2*n_groups)
                covariance = vcov(fit_lm)[ind,ind]
                meta = metafor::rma.mv( summary(fit_lm)$coefficients[ind, 1], V = covariance, method = "FE", test = "t" )
                res[nonSing2, 3] = meta$pval
              }, silent = TRUE)
              nonSing2 = nonSing2 + 1
            }
          }
          
        }
        parameter$sd = sd_re
        TypeOne = c(mean(res[,1] < 0.05, na.rm=TRUE),  mean(res[,2] < 0.05, na.rm=TRUE), mean(res[,3] < 0.05, na.rm=TRUE))
        parameter$TypeIold[i] = TypeOne[1]
        parameter$TypeInew[i] = TypeOne[2]
        parameter$TypeMeta[i] = TypeOne[3]
        return(parameter[i,])
      })
    tmp = do.call(rbind, tmp)
    tmp$balanced2 = scales::rescale(tmp$max/tmp$min - 1)
    results_list[[m]] = tmp
  }
  res_sd[[sd]] = results_list
}

results = do.call(rbind, res)
results50 = results
par(mfrow = c(1, 3))
plot(results$TypeIold~results$balanced, ylim = c(0.0, 0.1))
plot(results$TypeInew~results$balanced, ylim = c(0.0, 0.1))
plot(results$TypeMeta~results$balanced, ylim = c(0.0, 0.1))


results100 = do.call(rbind, res100)
par(mfrow = c(2, 3))
results100$balanced2 = scales::rescale(results100$max/results100$min - 1)


plot(TypeIold~balanced2,data=results100, ylim = c(0.0, 0.15))
plot(TypeInew~balanced2,data=results100, ylim = c(0.0, 0.15))
plot(TypeMeta~balanced2,data=results100, ylim = c(0.0, 0.15))


plot(TypeIold~balanced2,data=results100[results100$min > 20,], ylim = c(0.0, 0.15))
plot(TypeInew~balanced2,data=results100[results100$min > 20,], ylim = c(0.0, 0.15))
plot(TypeMeta~balanced2,data=results100[results100$min > 20,], ylim = c(0.0, 0.15))



par(mfrow = c(1, 3))

plot(TypeIold~balanced2,data=res_sd[[1]][[5]], ylim = c(0.0, 0.35))
plot(TypeInew~balanced2,data=res_sd[[1]][[5]], ylim = c(0.0, 0.35))
plot(TypeMeta~balanced2,data=res_sd[[1]][[5]], ylim = c(0.0, 0.35))


plot(TypeIold~balanced2,data=results50[results50$min > 10,], ylim = c(0.0, 0.15))
plot(TypeInew~balanced2,data=results50[results50$min > 10,], ylim = c(0.0, 0.15))
plot(TypeMeta~balanced2,data=results50[results50$min > 10,], ylim = c(0.0, 0.15))