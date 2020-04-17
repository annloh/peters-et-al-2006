# Load simulated data -----------------------------------------------------
sim_data<- readRDS("sim_data.rds")

# Load required functions -------------------------------------------------
source("src/test_functions.R")

# Analysis ----------------------------------------------------------------


# Perform egger test on simulated data ------------------------------------

results_egger <-   sim_data %>%
  group_by(job_id, scenario_id) %>%
    summarize(test_type ="egger",
              test_result = egger_test(or_sim, se_lnor, sig_threshold = 0.1))



# Perform Peters test on simulated data -----------------------------------

results_peters <- sim_data %>%
  group_by(job_id, scenario_id) %>%
    summarize(test_type ="peters",
              test_result = peters_test(or_sim, n, a, b, c, d, sig_threshold = 0.1))


# Combine results in long format for plotting -----------------------------

results_both <- bind_rows(results_egger, results_peters)

results_joined <- left_join(results_both, scenarios)


# add publication bias indicator to use as filter for error rates
results_joined %<>%
  mutate(pub_bias = (bias_percentage != 0 | bias_type == "p")) %>%
    ungroup()

# add error rate (type one error if pub_bias = false & power if pub_bias == true )

results_h0_true <- results_joined %>%
  filter(pub_bias== FALSE) %>%
    group_by(test_type, scenario_id) %>%
      summarise(error_rate = mean(test_result)) %>%
        left_join(. ,scenarios) %>%
          ungroup()


results_h0_false <- results_joined %>%
  filter(pub_bias== TRUE) %>%
    group_by(test_type, scenario_id) %>%
      summarise(error_rate = mean(test_result)) %>%
        left_join(. ,scenarios) %>%
          ungroup()


# Save data for plotting --------------------------------------------------

saveRDS(results_h0_true, "results_h0_true.rds")
saveRDS(results_h0_false, "results_h0_false.rds")


# Tabular results ---------------------------------------------------------

power_table <- results_h0_false %>%
    group_by(ma_size, odds_ratio,  bias_type, test_type) %>%
      summarize(power = mean(error_rate))
