########## __Figure 5 LM ########## 
test = readRDS("Results/C_results_mountain_unbalanced.RDS")
test$balanced2 = (test$max - test$min)/test$max

min_test = 5
max_test = 16

maximum_effect_size_typeI_LM = vector(length = max_test-min_test+1)
maximum_effect_size_typeI_LMM = vector(length = max_test-min_test+1)
for(potential_treshold in min_test:max_test){

  sub = test[test$moutain >= potential_treshold , ]
  sub$sd = scale(sub$sd)
  sub$nobs = scale(sub$nobs)
  sub$moutain = scale((sub$moutain)) 
  sub$balanced2 = scales::rescale((sub$balanced2)) 
  form = function(resp) paste0(resp, "~ sd + moutain + nobs + balanced2 + sd:moutain + sd:nobs + moutain:nobs + balanced2:sd + balanced2:moutain + balanced2:nobs")
  #form = function(resp) paste0(resp, "~ .^2")
  TypeLM = (lm(form("TypeOneLM_GM - 0.05"), data = sub))
  TypeLMM = (lm(form("TypeOneLMM - 0.05"), data = sub ))
  maximum_effect_size_typeI_LM[potential_treshold-min_test+1] = max(abs(TypeLM$coefficients[2:length(TypeLM$coefficients)]))
  maximum_effect_size_typeI_LMM[potential_treshold-min_test+1] = max(abs(TypeLMM$coefficients[2:length(TypeLMM$coefficients)]))
}


number_levels = min_test:max_test
typeI_smooth_LM = loess(maximum_effect_size_typeI_LM ~ number_levels)
typeI_smooth_LMM = loess(maximum_effect_size_typeI_LMM ~ number_levels) 

pdf("Figures/Fig_SX_appenidx.pdf", width = 9.4, height = 8)

par(mar=c(7, 7, 2, 1), mgp = c(5, 1, 0))
plot(predict(typeI_smooth_LM),  x= min_test:max_test, xlab = "Number of mountains",ylim =c(0,0.01), type = "l",
     ylab = "Maximum effect size", cex.lab =1.2, cex.axis = 1.2, col = "blue", las = 1, xpd = NA) 
lines(y = predict(typeI_smooth_LMM),  x= min_test:max_test, ylim =c(0,0.01), type = "l", col = "red")
points(y = maximum_effect_size_typeI_LM, x = number_levels, col = "blue")
points(y = maximum_effect_size_typeI_LMM, x = number_levels, col = "red")
legend(x = 13,y = 0.009 , legend=c("LM", "LMM"),
       col=c("blue", "red"), lty=1:2, cex=1.4, bty = "n")

dev.off()
