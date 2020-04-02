# load data

sim_data_1 <- readRDS("sim_data1.rds")


# meta-analysis level descriptives

#for each job_id, and each scenario_id get original_k, i_squared_unbiased_ i_squared_biased, every info in scenarios,
#publication bias indicator

# then for every row of this dataframe get eggers test and pub_bias test results by turning the tests into functions
# should work with sim_data[[job_id]][[scenario_id]]
# put the test result in a column called bias_test
# create another variable called test_type that can take "egger" or "peters"
# long form of this data frame should then be 1000 jobs * 400 scenarios * 2 methods = 8e+05


# Analysis

source("test_functions.R")


results_egger <-   sim_data_df %>% group_by(job_id,scenario_id) %>%
    summarize(test_type ="egger",
              test_result = egger_test(or_sim, se_lnor, sig_threshold = 0.1))



results_peters <-   sim_data_df %>% group_by(job_id,scenario_id) %>%
    summarize(test_type ="peters",
              test_result = peters_test(or_sim, n, a, b, c, d, sig_threshold = 0.1))



results_both <- bind_rows(results_egger, results_peters)



results_joined <- left_join(results_both, scenarios)


# add publication bias indicator to use as filter for error rates
results_joined %<>% mutate(pub_bias = (bias_percentage != 0 | bias_type == "p")) %>% ungroup()

# add error rate (type one error if pub_bias = false & power if pub_bias == true )

results_h0_true <- results_joined %>% filter(pub_bias== FALSE) %>%
                                        group_by(test_type, scenario_id) %>%
                                          summarise(error_rate = mean(test_result)) %>%
                                            left_join(. ,scenarios) %>% ungroup()



results_h0_false <- results_joined %>% filter(pub_bias== TRUE) %>%
                                        group_by(test_type, scenario_id) %>%
                                          summarise(error_rate = mean(test_result)) %>%
                                            left_join(. ,scenarios) %>% ungroup()


saveRDS(results_h0_true, "results_h0_true.rds")
saveRDS(results_h0_false, "results_h0_false.rds")

