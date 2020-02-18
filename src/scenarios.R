# define simulation scenarios

# simulation facors
scenario_id <- (1:400)

iter <- (1:1000)
bias_type <- c("p", "es")
bias_percentage <- c(0, 14, 40)
bias_strength <- c("moderate", "severe")
odds_ratio <- c(1, 1.2, 1.5, 3, 5) #5
heterogeneity <- c(0, 0.2, 1.5, 5) #4
ma_size <- c(6, 16, 30, 90) #4

scenarios <- expand.grid(bias_type = bias_type,
                         bias_percentage = bias_percentage,
                         bias_strength = bias_strength,
                         odds_ratio = odds_ratio,
                         heterogeneity =heterogeneity,
                         ma_size = ma_size,
                         stringsAsFactors = F,
                         KEEP.OUT.ATTRS = F)

# filter out wrong combinations
scenarios <- scenarios %>%
                  dplyr::mutate(bias_percentage = ifelse(bias_type == "p", NA, bias_percentage),
                                bias_strength = ifelse(bias_type == "es", NA, bias_strength)) %>%
                    unique() %>%
                      cbind(scenario_id = scenario_id, .)

# resetting rownames that got jumbled with the filtering
rownames(scenarios) <- NULL


# Fixed design elements
n_iter <- 1000

bias_table <- list(moderate =  list(p_table = c(0.05, 0.2, 0.5, 1),
                                    sec_table = c(1, 0.75, 0.5, 0.25)),
              severe = list(p_table = c(0.05, 0.2, 1),
                            sec_table = c(1, 0.75, 0.25)))

prob_cg_distr <- expr(runif(1, min = 0.3, max = 0.7))
