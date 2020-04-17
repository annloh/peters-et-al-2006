# Code pertaining to the replication attempt of
# Peters et al 2006

source('src/dependencies.R') # load dependencies
source('src/data_generation_functions.R') # load helper functions for data generation
source('src/scenarios.R') # load compile_scenarios() functions
source('src/run_sim.R') # load run_sim() function

# Define simulation parameters as specified by Peters et al 2006 ----------------


# Fixed design parameters -------------------------------------------------

n_iter <- 1000

bias_table <- list(
              moderate =  list(p_table = c(0.05, 0.2, 0.5, 1),
                               sec_table = c(1, 0.75, 0.5, 0.25)),
              severe = list(p_table = c(0.05, 0.2, 1),
                            sec_table = c(1, 0.75, 0.25))
              )

prob_cg_distr <- function(){runif(1, min = 0.3, max = 0.7)}
n_cg_distr <- function(){exp(rnorm(1, mean = 5, sd = 0.3)) %>% round()}

# Variable design parameters ----------------------------------------------

bias_type <- c("p", "es")
bias_percentage <- c(0, .14, .40)
bias_strength <- c("moderate", "severe")
odds_ratio <- c(1, 1.2, 1.5, 3, 5) #5
heterogeneity <- c(0, 0.2, 1.5, 5) #4
ma_size <- c(6, 16, 30, 90) #4

# Compile dataframe with scenarios ----------------------------------------

scenarios <- compile_scenarios(bias_type = bias_type,
                               bias_percentage = bias_percentage,
                               bias_strength = bias_strength,
                               odds_ratio = odds_ratio,
                               heterogeneity = heterogeneity,
                               ma_size = ma_size,
                               prob_cg_distr = prob_cg_distr,
                               n_cg_distr = n_cg_distr,
                               bias_table = bias_table)

saveRDS(scenarios, file = "scenarios.rds")

# Run simulation ----------------------------------------------------------

sim_data<- run_sim(iteration_range = 1:n_iter,    #please change for testing
                          scenarios = scenarios)

saveRDS(sim_data, file = "sim_data.rds")
