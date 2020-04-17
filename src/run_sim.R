#' Run a prespecified simulation subset
#'
#' @param iteration_range Numerical vector of iterations to be run
#' @param scenarios dataframe specifying simulation scenarios i.e.
#'                   constellations of simulation paramters
#' @param scenario_selection Numerical vector indicating which rows of the
#'                           scenarios dataframe should be simulated.
#'                           Defaults to entire dataframe
#'
#' @return Returns a list of dataframes containing the simulated meta-analyses

run_sim <- function(iteration_range,
                    scenarios,
                    scenario_selection = 1:nrow(scenarios)){
  sim_data <- list()

  for (i in iteration_range) {
  start_time <- Sys.time()
  print(paste("Busy with iteration", i))
    job_id <- i

    sim_data[[i]] <- purrr::pmap_dfr(scenarios[scenario_selection,],
                                     function(scenario_id,
                                              bias_type,
                                              bias_percentage,
                                              bias_strength,
                                              odds_ratio,
                                              heterogeneity,
                                              ma_size,
                                              prob_cg_distr,
                                              n_cg_distr,
                                              bias_table) {
      generate_meta_analysis(
                  job_id = job_id,
                  scenario_id = scenario_id, # inputs first column
                  bias_type = bias_type,
                  bias_percentage = bias_percentage,
                  bias_strength = bias_strength,
                  odds_ratio = odds_ratio,
                  heterogeneity = heterogeneity,
                  ma_size = ma_size,
                  prob_cg_distr = prob_cg_distr,
                  n_cg_distr = n_cg_distr,
                  bias_table = bias_table
      )
    })
  print(Sys.time() - start_time)
  }
  sim_data_df <- sim_data %>% do.call(rbind, .)

}
