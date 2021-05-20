# # getParsBT2 = function(samples) {
# #   ll = function(par) {
# #     mean = par[1]
# #     sd = par[2]
# #     if(sd < 0.0) return(Inf)
# #     ll = sum(sapply(1:nrow(samples), function(i) sum(dnorm(rnorm(1000, samples[i,1], samples[i,2]), mean, sd, log =TRUE))))
# #     return(-ll)
# #   }
# #   pp = optim(c(0.1, 0.1), ll)$par
# #   return(c(pp[1], pp[2]/sqrt(nrow(samples)-1)))
# # }
# 
# 
# grand_mean = function(fit, mountain, beta, weighted = TRUE, z_statistic = FALSE) {
#   
#   ind = (mountain+1):(2*mountain)
#   covariance = vcov(fit)[ind,ind]
#   effect = summary(fit)$coefficients[ind, 1]
#   
#   if(!weighted) weights = rep(1/mountain, mountain)
#   else weights = apply(covariance, 1,sum)/sum(apply(covariance, 1,sum))
#   effect_sizes = effect
#   eff = weighted.mean(effect_sizes, weights)
#   var1 = 0
#   for(i in 1:mountain){
#     for(j in 1:mountain){
#       var1 = var1 + covariance[i,j]
#     }
#   }
#   var1 = var1/mountain^2
#   var2 = (1/(mountain*(mountain-1)))*sum((effect_sizes-mean(effect_sizes))^2)
#   se = sqrt(var1 + var2)
#   
#   if(z_statistic) {
#     p_value = 2*pnorm(abs(eff/se), lower.tail = FALSE)
#     confs = cbind( eff -1.96*se, eff + 1.96*se)
#   } else {
#     p_value = 2*pt(abs(eff/se),df=mountain-1, lower.tail = FALSE)
#     bound = qt(0.975, df = mountain-1)
#     confs = cbind( eff - bound*se, eff + bound*se) 
#   }
#   return(c(eff, p_value, se, as.integer(beta > confs[1,1] & beta < confs[1,2])))
# }
# 
# mountains = c(2, 4, 10, 50)
# beta = 0.0
# beta0= 0.0
# 
# ss = c(50, 1000, 3000)
# 
# sds = c(0.1, 1.0, 5.0)
# 
# try({snow::stopCluster(cl)}, silent = TRUE)
# cl = snow::makeCluster(24L)
# .l = snow::clusterExport(cl, list("grand_mean", "ss", "mountains", "beta", "beta0", "sds"))
# 
# results_mountain = list("vector", length(mountains))
# 
# for(which_mountain in 1:length(mountains)) {
#   mountain = mountains[which_mountain]
#   print(mountain)
#   
#   results_sds = list("vector", length(sds))
#   for(which_sd in 1:length(sds)) {
#     sd = sds[which_sd]
#     
#     print(sd)
#     
#     results = vector("list", length(ss))
# 
#     for(k in 1:length(ss)) {
#       
#       n = ss[k]
#       n_obs = mountain*n
#       groups = rep(1:mountain, n)
#       
#       
#       X = runif(n_obs, -1, 1)
#       
#       snow::clusterExport(cl, list("X", "k", "n", "n_obs", "groups", "sd", "mountain"))
#       
#       res= 
#         snow::parSapply(cl, 1:2000, function(egal) {
#           
#           
#           slopes = rnorm(mountain, mean = beta, sd = sd)
#           inters = rnorm(mountain, mean = beta0, sd = sd)
#           
#           Y = sapply(1:n_obs, function(i) X[i]*slopes[groups[i]] + inters[groups[i]] ) + rnorm(n_obs, sd = 0.2)
#           
#           GG = as.factor(groups)
#           
#           ind = (mountain+1):(2*mountain)
#           m_try = lm(Y~0+X*GG-X)
#           sum = summary(m_try)
#           covariance = vcov(m_try)[ind,ind]
#           
#           
#           samples = cbind(sum$coefficients[ind,1], diag(covariance))
#           
#           #r1=getParsBT2 (samples)
#           r2=grand_mean (m_try, mountain, 0, df = n_obs-length(coef(m_try)))
#           #r3=getParsJO (sum$coefficients[ind,1], covariance, TRUE)
#           
#           #get_p = function(r) 2*pnorm(abs(r[1]/r[2]), lower.tail = FALSE)
#           #return(c(sapply(list(r1, r2, r3), get_p), r1[1], r2[1], r3[1]))
#           return(r2)
#         })
#       results[[k]] = t(res)
#       
#     }
#     
#     results_sds[[which_sd]] = results
#   }
#   results_mountain[[which_mountain]] = results_sds
#   
#   #saveRDS(results_mountain, file = "Results/results_grand_mean.RDS")
# }
# 
# snow::stopCluster(cl)
