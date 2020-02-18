#---


pred_SE <- cbind(1, SE_lnOR)

inverse_sample_size <- 1/(2*n)

alt_weights <- (1/(a + b)) + (1/(c + d))
# random effect


#random effects model

# fixed effects models
# the -1 in the formular removes the intercept. To artificially add it and make sure it is also weighted
# by the variance add it manually to the predictor

fit_egger  <- lm(lnOR_sim  ~ pred_SE  - 1, weights = 1/var_within)
summary(fit_egger)

std_ES <- lnOR_sim/SE_lnOR
precision <- 1/SE_lnOR

fit_egger  <- lm(std_ES  ~ precision)

fit_egger  <- glm(lnOR_sim  ~ SE_lnOR, weights = 1/var_within)
summary(fit_egger)
#The above are equivalent so I seem to have done it right :-)

fit_alternative <- lm(lnOR_sim  ~ inverse_sample_size, weights = 1/alt_weights)
summary(fit_alternative)

fit_alternative <- glm(lnOR_sim  ~ inverse_sample_size, weights = 1/alt_weights)
summary(fit_alternative)

