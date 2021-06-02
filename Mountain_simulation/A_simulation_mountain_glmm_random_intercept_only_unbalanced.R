library(lme4)
library(lmerTest)
library(snow)
library(glmmTMB)
set.seed(1)

## Note:
#The first simulation (Simulation of GLMM with Temperature effect) tests if the effect is significant when there is an effect 
#The second simulation (Simulation of GLMM without Temperature effect) tests if the effect is significant, when there is an effect

################ Helper functions  ################ 
# helper functions to extracts model properties 


extract_results = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients$cond["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients$cond["x","Estimate"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients$cond["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$cond$group,"stddev")["(Intercept)"],
      #attr(summary(fit_lmm)$varcor$cond$group,"stddev")["x"],
      as.integer(!fit_lmm$sdr$pdHess),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}
extract_results_lme4 = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group,"stddev")["(Intercept)"],
      #attr(summary(fit_lmm)$varcor$group.1,"stddev")["x"],
      !is.null(fit_lmm@optinfo$conv$lme4$messages),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}
inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}

# set up the cluster and export variables as well as functions to the cluster 

cl = snow::makeCluster(7L) # reduce the number of cores if you have not 7 physical CPU cores
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest);library(glmmTMB); number_experiments =5000})
snow::clusterExport(cl, list("extract_results","extract_results_lme4", "inv.logit"), envir = environment())


## loop over different numbers of data points in each level

for(n_each in c(100, 25, 50, 200, 500)) {
  # export the number of data points in each level to the cluster as required 
  # run parameters for the simulations 
  snow::clusterExport(cl, list("n_each"), envir = environment())
  
  system.time({
    result_list = 
      snow::parLapply(cl, 2:8, function(number_groups)  {
        
        n_groups <- number_groups
        
        ## setting up matrices to store results of multiple runs 
        
        results_w_lme4_ml = results_wo_lme4_ml = matrix(nrow = number_experiments, ncol = 10)
        colnames(results_w_lme4_ml) = c("estimate_intercept","estimate_effect", 
                                        "p_value_intercept", "p_value_effect",
                                        "se_intercept", "se_effect",
                                        "stddev_randeff_inter", "Singularity",
                                        "Int_in_conf",  "Slope_in_conf")
        colnames(results_wo_lme4_ml) = colnames(results_w_lme4_ml)
        
        
        results_w_glmmTMB_ml = results_wo_glmmTMB_ml = results_w_glmmTMB_reml = results_wo_glmmTMB_reml = matrix(nrow = number_experiments, ncol = 10)
        colnames(results_w_glmmTMB_ml) = c("estimate_intercept","estimate_effect", 
                                           "p_value_intercept", "p_value_effect",
                                           "se_intercept", "se_effect",
                                           "stddev_randeff_inter", "Singularity",
                                           "Int_in_conf",  "Slope_in_conf")
        colnames(results_wo_glmmTMB_ml) = colnames(results_w_glmmTMB_ml)
        colnames(results_w_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
        colnames(results_wo_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
        
        
        results_w_lm = results_wo_lm = results_wo_lm_wo_grouping  = results_w_lm_wo_grouping= matrix(nrow = number_experiments, ncol = 4)
        colnames(results_w_lm) = c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
        colnames(results_wo_lm) = colnames(results_w_lm)
        colnames(results_wo_lm_wo_grouping) = colnames(results_w_lm)
        colnames(results_w_lm_wo_grouping) = colnames(results_w_lm)
        
        for(experiment in 1:number_experiments){
          
          ### Raw environmental predictors (Intercept and Temperature) ###
          # the temperature is drawn from a uniform distribution as we want to simulate 
          # an altidudinal gradient and the intercept is kept constant at 1 
          n = (number_groups)*n_each
          
          ### Environmental predictors (Intercept and Temperature) ###
          x <- runif(n, -1, 1) # Temperature
          X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
          sd_randeff = 0.1 # sd for random effects
          
          
          ################ Simulation of GLMM with Temperature effect  ################ 
          # fixed effects
          beta0 = 0.4  # Intercept: the mean reproductive success 
          beta = 0.4   # Temperature effect meaning when the temperature increases 1 degree the respective variable also does 
          
          # random intercept is sampled around the mean beta0
          
          g <- rep(1:n_groups, n_each) # Grouping variable (mountain range)
          continue = TRUE
          while(continue) {
            g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = runif(n_groups, 0.1/(n_groups/2), 0.9/(n_groups/2)))
            if(min(table(g)) > 2) continue = FALSE
          }
          group <-  as.factor(g)
          randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff) # random intercept
          
          # calculate linear response, different intercept for each mountain range
          mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]], beta)) 
          
          # apply inverse logit link and sample random errors from a Binomial distribution
          y <- rbinom(n, size = 1, prob = inv.logit(mu))
          
          
          ################ Fitting: lme4, glmmTMB, and LM ################ 
          # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB
          # lme4 supports only MLE for GLMMs
          
          # lme4 - MLE
          try({
            fit_lmm <- glmer(y ~ x  + (1 | group), family = binomial)
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - 1.96*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + 1.96*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - 1.96*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + 1.96*summ$coefficients[2,"Std. Error"])
                          )
            rownames(confs) = c("(Intercept)", "x")
            results_w_lme4_ml[experiment, ] = extract_results_lme4(fit_lmm, confs, beta, beta0)
          }, silent=TRUE)
          
          # glmmTMB - MLE
          try({
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), family = binomial, REML = FALSE) 
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
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), family = binomial, REML = TRUE)
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
            fit_lm = glm(y ~ x + group, family = binomial)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_w_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)
          
          # linear model w/o mountain range as grouping variable
          try({
            fit_lm = glm(y ~ x, family = binomial)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_w_lm_wo_grouping[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)
          
          ################ Simulation of GLMM without Temperature effect  ################ 
          # fixed effects
          beta0 = 0.4  # Intercept
          beta = 0.0   # Temperature, now set to zero
          
          # random effects
          g <- rep(1:n_groups, n_each) # Grouping variable (mountain range)
          continue = TRUE
          while(continue) {
            g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = runif(n_groups, 0.1/(n_groups/2), 0.9/(n_groups/2)))
            if(min(table(g)) > 2) continue = FALSE
          }
          group <-  as.factor(g)
          randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff) # random intercept
          
          # calculate linear response, different intercept for each mountain range
          mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]], beta)) 
          
          # apply inverse logit link and sample random errors from a Binomial distribution
          y <- rbinom(n, size = 1, prob = inv.logit(mu))
          
          
          ################ Fitting: lme4, glmmTMB, and LM ################ 
          # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB
          # lme4 supports only MLE for GLMMs
          
          try({
            fit_lmm <- glmer(y ~ x  + (1 | group), family = binomial) 
            summ = summary(fit_lmm)
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - 1.96*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + 1.96*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - 1.96*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + 1.96*summ$coefficients[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_lme4_ml[experiment, ] = extract_results_lme4(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
            
          # glmmTMB - MLE
          try({
          fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), family = binomial, REML = FALSE) 
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
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), family = binomial, REML = TRUE) 
            summ = summary(fit_glmmTMB)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                          cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_glmmTMB_reml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
          }, silent = TRUE)
          
          # linear model w/ mountain range as grouping variable
          try({
            fit_lm = glm(y ~ x+group, family = binomial)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_wo_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)
          
          # linear model w/o mountain range as grouping variable
          try({
            fit_lm = glm(y ~ x, family = binomial)
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
                    results_w_glmmTMB_reml = data.frame(results_w_glmmTMB_reml), 
                    results_wo_glmmTMB_reml = data.frame(results_wo_glmmTMB_reml),
                    results_wo_lm_wo_grouping = data.frame(results_wo_lm_wo_grouping),
                    results_w_lm_wo_grouping = data.frame(results_w_lm_wo_grouping)
        ))
      })
  })
  saveRDS(result_list, file = paste0("Results/results_mountain_glmm_random_intercept_only_",n_each,"_unbalanced.Rds"))
}



