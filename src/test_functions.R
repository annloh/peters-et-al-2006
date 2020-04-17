
# Test definitions --------------------------------------------------------



# Egger test --------------------------------------------------------------

#' Perform Egger test
#'
#' @param or_sim Simulated odds ratio (as opposed to the OR give as a simulation parameter)
#' @param se_lnor standard error of the lnOR computed from simulated confusion matrix a,b,c,d
#' @param sig_threshold threshold of significace defaults to 0.1 as in the original simulation
#'
#' @return A logical value indicating whether the p value was below the sig_threshold

egger_test <- function(or_sim, se_lnor,
                       sig_threshold = 0.1){
  lnor_sim <- log(or_sim)
  std_es <- lnor_sim/se_lnor
  precision <- 1/se_lnor
  fit_egger  <- lm(std_es  ~ precision)

  test_result <- summary(fit_egger)$coefficients["precision","Pr(>|t|)"] < sig_threshold
}


# Peters test -------------------------------------------------------------

#' Perform Peters Test
#'
#' @param or_sim or_sim Simulated odds ratio (as opposed to the OR give as a simulation parameter)
#' @param n Number of aprticipants in intervention and controlgroup (i.e. half the sample size)
#' @param a a from confusion matrix (i.e. number of events in exposed group)
#' @param b b from confusion matrix (i.e. number of events in control group)
#' @param c c fom confusion matrix (i.e. number of non-events in exposed group)
#' @param d d from confusion matrix (i.e. number of non events in control group)
#' @param sig_threshold threshold of significace defaults to 0.1 as in the original simulation
#'#'
#' @return A logical value indicating whether the p value was below the sig_threshold

peters_test <- function(or_sim,
                        n,
                        a,
                        b,
                        c,
                        d,
                        sig_threshold = 0.1){
  lnor_sim <- log(or_sim)
  weights <- 1/((1/(a + b)) + (1/(c + d)))
  inv_size <- 1/(2*n)
  fit_peters <- lm(lnor_sim  ~ inv_size, weights = weights)
  test_result <- summary(fit_peters)$coefficients["inv_size","Pr(>|t|)"] < sig_threshold
}
