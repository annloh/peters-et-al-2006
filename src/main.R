# Code pertaining to the replication attempt of
# Peters et al 2006

source('src/dependencies.R') # load dependencies
source('src/utils.R') # load utility fuctions
source('src/scenarios.R') # load df with simulation scenarios

run_sim <- function(iteration_range, scenarios, scenario_selection = 1:nrow(scenarios)){
  sim_data <- list()

  for (i in iteration_range) {
  start_time <- Sys.time()
  print(paste("Busy with iteration", i))
    job_id <- i

    sim_data[[i]] <- purrr::pmap_dfr(scenarios[scenario_selection,],
                                     function(scenario_id, bias_type, bias_percentage,
                                              bias_strength, odds_ratio, heterogeneity,
                                              ma_size) {
      generate_ma(
                  job_id = job_id,
                  scenario_id = scenario_id, # inputs first column
                  bias_type = bias_type,
                  bias_percentage = bias_percentage,
                  bias_strength = bias_strength,
                  odds_ratio = odds_ratio,
                  heterogeneity = heterogeneity,
                  ma_size = ma_size,
                  prob_cg_distr = prob_cg_distr #change this to..8
      )
    })
  print(Sys.time() - start_time)
    }
return(sim_data)
}

sim_data_trial <- run_sim(iteration_range =  1:50,
                          scenarios = scenarios)


sim_data_df <-  sim_data_trial %>% do.call(rbind, .)

names(sim_data_df)




saveRDS(sim_data_df, file = "sim_data_df.rds")



