library(lme4)
library(lmerTest)
library(snow)
library(glmmTMB)
set.seed(1)

## Note:
#The first simulation (Simulation of LMM with Temperature effect) tests if the effect is significant when there is an effect 
#The second simulation (Simulation of LMM without Temperature effect) tests if the effect is significant, when there is an effect

################ Helper functions  ################ 
# helper functions to extracts model properties 
extract_results_t = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group, "stddev")["(Intercept)"],
      attr(summary(fit_lmm)$varcor$group, "stddev")["x"],
      !is.null(fit_lmm@optinfo$conv$lme4$messages),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}

extract_results = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients$cond["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients$cond["x","Estimate"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients$cond["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$cond$group,"stddev")["(Intercept)"],
      attr(summary(fit_lmm)$varcor$cond$group,"stddev")["x"],
      as.integer(!fit_lmm$sdr$pdHess),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}

grand_mean = function(fit, mountain, beta, weighted = TRUE, z_statistic = FALSE) {
  
  ind = (mountain+1):(2*mountain)
  covariance = vcov(fit)[ind,ind]
  effect = summary(fit)$coefficients[ind, 1]
  
  if(!weighted) weights = rep(1/mountain, mountain)
  else weights = apply(covariance, 1,sum)/sum(apply(covariance, 1,sum))
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
  return(c(eff, p_value, se, as.integer(beta > confs[1,1] & beta < confs[1,2])))
}
# set up the cluster and export variables as well as functions to the cluster 


cl = snow::makeCluster(7L) # reduce the number of cores if you have not 7 physical CPU cores
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest);library(glmmTMB); number_experiments =5000})
snow::clusterExport(cl, list("extract_results","extract_results_t", "grand_mean"), envir = environment())

# first we vary the standard deviation of the random effect 
for(n_each in c( 50, 100, 200, 500)) {
  snow::clusterExport(cl, list("n_each"), envir = environment())
  for(sd_re in c(0.1, 0.01, 0.5, 2.0)) {
    # export the standard deviation of the random effect to the cluster  
    # as a run parameter for the simulations 
    snow::clusterExport(cl, list("sd_re"), envir = environment())
    system.time({
      result_list = 
        snow::parLapply(cl, 2:8, function(number_groups)  {
          
          # set the number of observations per group to 50 
          
          n_each <- n_each
          n_groups <- number_groups
          
          # set up matrices to store the results of the different runs 
          
          results_w_lme4_ml = results_wo_lme4_ml = results_w_lme4_reml = results_wo_lme4_reml = matrix(nrow = number_experiments, ncol = 11)
          colnames(results_w_lme4_ml) = c("estimate_intercept","estimate_effect", 
                                  "p_value_intercept", "p_value_effect",
                                  "se_intercept", "se_effect",
                                  "stddev_randeff_inter","stddev_randeff_x", "Singularity",
                                  "Int_in_conf",  "Slope_in_conf")
          colnames(results_wo_lme4_ml) = colnames(results_w_lme4_ml)
          colnames(results_w_lme4_reml) = colnames(results_w_lme4_ml)
          colnames(results_wo_lme4_reml) = colnames(results_w_lme4_ml)
          
          
          results_w_glmmTMB_ml = results_wo_glmmTMB_ml = results_w_glmmTMB_reml = results_wo_glmmTMB_reml = matrix(nrow = number_experiments, ncol = 11)
          colnames(results_w_glmmTMB_ml) = c("estimate_intercept","estimate_effect", 
                                       "p_value_intercept", "p_value_effect",
                                       "se_intercept", "se_effect",
                                       "stddev_randeff_inter","stddev_randeff_x", "Singularity",
                                       "Int_in_conf",  "Slope_in_conf")
          colnames(results_wo_glmmTMB_ml) = colnames(results_w_glmmTMB_ml)
          colnames(results_w_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
          colnames(results_wo_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
          
          
          results_w_lm = results_wo_lm = results_wo_lm_wo_grouping = results_w_lm_wo_grouping = matrix(nrow = number_experiments, ncol = 4)
          colnames(results_w_lm) = c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
          colnames(results_wo_lm) = colnames(results_w_lm)
          colnames(results_wo_lm_wo_grouping) = colnames(results_w_lm)
          colnames(results_w_lm_wo_grouping) = colnames(results_w_lm)
          
          
          for(experiment in 1:number_experiments){
            
            ################  The data generating process ################ 
            # Number of overall observations is equal to 50 observations for each mountain range 
            n = (number_groups)*n_each
            
            ### Raw environmental predictors (Intercept and Temperature) ###
            # the temperature is drawn from a uniform distribution as we want to simulate 
            # an altidudinal gradient and the intercept is kept constant at 1 
            x <- runif(n, -1, 1) # Temperature
            X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
            
            sd_randeff = sd_re # sd for random effects
            
            ################ Simulation of LMM with Temperature effect  ################ 
            # fixed effects
            beta = 0.4    # Temperature effect meaning when the temperature increases 1 degree the respective variable also does 
            beta0 = 10.0   # Intercept: the mean height of a reporductive plant  
            
            # random effect sizes are sampled around the fixed effects with no correlation between the 
            # random intercept and random slope
            #g <- rep(1:n_groups, n_each) # Grouping variable (mountain range)
            continue = TRUE
            while(continue) {
              g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = runif(n_groups, 0.1/(n_groups/2), 0.9/(n_groups/2)))
              if(min(table(g)) > 2) continue = FALSE
            }
            group <-  as.factor(g)
            randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff) # random intercept
            randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)     # random slope    # random slope
            
            # calculate linear response, different intercept and slope for each mountain range
            mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]]))
            
            # sample random error from a Normal distribution
            sigma <- 0.5
            y <- rnorm(n, mu, sd = sigma) 
            
            
            ################ Fitting: lme4, glmmTMB, and LM ################ 
            # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB and lme4
            
            # lme4 - MLE
            try({
              fit_lmm <- lmer(y ~ x  + (x | group), REML = FALSE) 
              summ = summary(fit_lmm)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                  summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                            cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                  summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
                            )
              rownames(confs) = c("(Intercept)", "x")
              results_w_lme4_ml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
            }, silent = TRUE)
            
            
            # lme4 - REML
            try({
              fit_lmm <- lmer(y ~ x  + (x | group), REML = TRUE) 
              summ = summary(fit_lmm)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                  summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                            cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                  summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_w_lme4_reml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
            }, silent = TRUE)
              
            # glmmTMB - MLE
            try({
              fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), REML = FALSE) 
              summ = summary(fit_glmmTMB)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                  summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                            cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                  summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_w_glmmTMB_ml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
            }, silent = TRUE)
            
            # glmmTMB - REML
            try({
              fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), REML = TRUE)
              summ = summary(fit_glmmTMB)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                  summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                            cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                  summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_w_glmmTMB_reml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
            }, silent = TRUE)
            
            # linear model w/ mountain range as grouping variable
            try({
              fit_lm = lm(y ~ 0+x*group-x)
              results_w_lm[experiment, ] = grand_mean(fit_lm, n_groups, beta = beta)
            }, silent = TRUE)
            
            
            
            # linear model w/o mountain range as grouping variable
            try({
              fit_lm = lm(y ~ x)
              summ = summary(fit_lm)
              confs = confint(fit_lm)
              results_w_lm_wo_grouping[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
            }, silent = TRUE)
            
            
            
            
            ################ Simulation of LMM without Temperature effect  ################ 
            # fixed effects
            beta0 = 10.0  # Intercept
            beta = 0.0    # Temperature, now set to zero
            
            # random intercept
            continue = TRUE
            while(continue) {
              g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = runif(n_groups, 0.1/(n_groups/2), 0.9/(n_groups/2)))
              if(min(table(g)) > 2) continue = FALSE
            }
            group <-  as.factor(g)
            randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff) # random intercept
            randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)     # random slope    # random slope
            
            # calculate linear response, different intercept and slope for each mountain range
            mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]]))
            
            # sample random errors from a Normal distribution
            sigma <- 0.5
            y <- rnorm(n, mu, sd = sigma) 
            
            
            ################ Fitting: lme4, glmmTMB, and LM ################ 
            # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB and lme4
            
            # lme4 - MLE
            try({
              fit_lmm <- lmer(y ~ x  + (x | group), REML = FALSE) 
              summ = summary(fit_lmm)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                  summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                            cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                  summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_wo_lme4_ml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
            }, silent = TRUE)
            
            # lme4 - REML
            try({
              fit_lmm <- lmer(y ~ x  + (x | group), REML = TRUE) 
              summ = summary(fit_lmm)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                  summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                            cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                  summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_wo_lme4_reml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
            }, silent = TRUE)
            
            # glmmTMB - MLE
            try({
              fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), REML = FALSE) 
              summ = summary(fit_glmmTMB)
              
              # calculate confidence intervals for the temperature effect
              confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                  summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                            cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                  summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_wo_glmmTMB_ml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
            }, silent = TRUE)
            
            # glmmTMB - REML
            try({
              fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), REML = TRUE) 
              summ = summary(fit_glmmTMB)
              confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                  summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                            cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                  summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
              )
              rownames(confs) = c("(Intercept)", "x")
              results_wo_glmmTMB_reml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
            }, silent = TRUE)  
            
            # linear model w mountain range as grouping variable
            try({
              fit_lm = lm(y ~ 0+x*group-x)
              results_wo_lm[experiment, ] = grand_mean(fit_lm, n_groups, beta)
            }, silent = TRUE)  
            
            # linear model w/o mountain range as grouping variable
            try({
              fit_lm = lm(y ~ x)
              summ = summary(fit_lm)
              confs = confint(fit_lm)
              results_wo_lm_wo_grouping[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
            }, silent = TRUE)  
          }
          
          return(list(results_w_lme4_ml = data.frame(results_w_lme4_ml), 
                      results_wo_lme4_ml = data.frame(results_wo_lme4_ml),
                      results_w_glmmTMB_ml = data.frame(results_w_glmmTMB_ml), 
                      results_wo_glmmTMB_ml = data.frame(results_wo_glmmTMB_ml),
                      results_w_lm = data.frame(results_w_lm),
                      results_wo_lm = data.frame(results_wo_lm),
                      results_w_lme4_reml = data.frame(results_w_lme4_reml), 
                      results_wo_lme4_reml = data.frame(results_wo_lme4_reml),
                      results_w_glmmTMB_reml = data.frame(results_w_glmmTMB_reml), 
                      results_wo_glmmTMB_reml = data.frame(results_wo_glmmTMB_reml),
                      results_w_lm_wo_grouping = data.frame(results_w_lm_wo_grouping),
                      results_wo_lm_wo_grouping = data.frame(results_wo_lm_wo_grouping)
                      ))
        })
    })
    saveRDS(result_list, file = paste0("Results/results_mountain_lmm_", sd_re, "_",n_each, "_unbalanced.Rds"))
  }
}


