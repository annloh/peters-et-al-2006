# analysis function

egger_test <- function(or_sim, se_lnor, sig_threshold){
                lnor_sim <- log(or_sim)
                std_es <- lnor_sim/se_lnor
                precision <- 1/se_lnor
                fit_egger  <- lm(std_es  ~ precision)

                test_result <- summary(fit_egger)$coefficients["precision","Pr(>|t|)"] < sig_threshold
      }


peters_test <- function(or_sim, n, a, b, c, d, sig_threshold){
                          lnor_sim <- log(or_sim)
                          weights <- 1/((1/(a + b)) + (1/(c + d)))
                          inv_size <- 1/(2*n)
                          fit_peters <- lm(lnor_sim  ~ inv_size, weights = weights)
                          test_result <- summary(fit_peters)$coefficients["inv_size","Pr(>|t|)"] < sig_threshold
                          }
