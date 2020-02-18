# Code pertaining to the replication attempt of
# Peters et al 2006

source('src/dependencies.R') # load dependencies
source('src/utils.R') # load utility fuctions
source('src/scenarios.R') # load df with simulation scenarios


job_id <- 1
range <- 135

purrr::pmap(scenarios[range,], ~{
  start_time <-  Sys.time()
  generate_ma(job_id = job_id,
              scenario_id = ..1, #inputs first column
              bias_type = ..2,
              bias_percentage = ..3,
              bias_strength = ..4,
              odds_ratio = ..5,
              heterogeneity = ..6,
              ma_size = ..7,
              prob_cg_distr = prob_cg_distr)
})


