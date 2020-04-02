# Analysis

egger_test <- sim_data %>% map(~ .x %>% map_lgl(~{lnor_sim <- log(.$or_sim)
                            std_es <- lnor_sim/.$se_lnor
                            precision <- 1/.$se_lnor
                            fit_egger  <- lm(std_es  ~ precision)

                            summary(fit_egger)$coefficients["precision","Pr(>|t|)"] < 0.1
}))
df_test <- bind_cols(egger_test)

alt_test <- sim_data %>% map(~ .x %>% map_lgl(~{lnor_sim <- log(.$or_sim)
                            weights <- 1/((1/(.$a + .$b)) + (1/(.$c + .$d)))
                            inv_size <- 1/(2*.$n)
                            fit_alt  <- lm(lnor_sim  ~ inv_size, weights = weights)

                            summary(fit_alt)$coefficients["inv_size","Pr(>|t|)"] < 0.1
})) %>% bind_cols()

# actual presence or absence of publication bias
pub_bias <- scenarios$bias_percentage != 0 | is.na(scenarios$bias_percentage)

outcomes <- cbind(df_test,pub_bias, scenarios)

case_wise_type_1_error <- outcomes %>% filter(pub_bias == FALSE, bias_type == "p") %>%
  mutate(error_rate = rowMeans(.[,1:10]))

case_wise_power <- outcomes %>% filter(pub_bias == TRUE) %>%
  mutate(error_rate = rowMeans(.[,1:10]))

# plotting
plot_type_1 <-  case_wise_type_1_error %>%
  ggplot( aes(x=factor(ma_size),
              y=error_rate,
              group = factor(odds_ratio),
              color = factor(odds_ratio))) +
  geom_point(fun = "mean", stat = "summary")

  stat_summary(fun="mean", geom="point")


plot_power <-  function(facet){case_wise_power %>%  ggplot( aes(x=factor(ma_size), y=error_rate, group = factor(odds_ratio), color = factor(odds_ratio))) +
  stat_summary(fun.y="mean", geom="point") +
  facet_grid(cols = vars(factor(facet)))
}

plot_power(bias_type)
plot_power(heterogeneity)

# performance eggers regression test
true_positive <- egger_test %>% map(~{(pub_bias == TRUE) & (.x == TRUE)})
mean(true_positive)
true_negative <- (pub_bias == FALSE) & (egger_test == FALSE)
mean(true_negative)

# type 1 error = false positive
false_positive <- egger_test %>% map(~{(pub_bias == FALSE) & (.x == TRUE)})
type_1_rate <- false_positive %>% map(~{sum(.x)/sum((pub_bias == FALSE))})

false_negative <- (pub_bias == TRUE) & (egger_test == FALSE)
mean(false_negative)
# power The power of a binary hypothesis test is the probability
# that the test rejects the null hypothesis (H0) when a specific
# alternative hypothesis (H1) is true
# power = prob(rejecting H_0 | H_1 = TRUE)


power <- mean(true_positive)

# performance alternative test

bias_test_alt <- sim_data %>% map_lgl(~{lnor_sim <- log(.$or_sim)
                                      std_es <- lnor_sim/.$se_lnor
                                      precision <- 1/.$se_lnor
                                      fit_egger  <- lm(std_es  ~ precision)

                            summary(fit_egger)$coefficients["precision","Pr(>|t|)"] < 0.1
})
