#' Compile a dataframe of simulation scenarios
#'
#' @param bias_type Charakter vector idicating bias types
#' @param bias_percentage Numeric vector indicating bias percentage
#' @param bias_strength Character vector indicating bias strength
#' @param odds_ratio Numeric vector of odds ratios
#' @param heterogeneity Numeric vector of heterogeneities (as percentage of between study variance)
#' @param ma_size Numeric vector indicaing the number of individual studies per meta-analysis
#'
#' @return Returns dataframe with simulation scenarios

compile_scenarios <- function(bias_type,
                              bias_percentage,
                              bias_strength,
                              odds_ratio,
                              heterogeneity,
                              ma_size){

  scenarios <- expand.grid(bias_type = bias_type,
                           bias_percentage = bias_percentage,
                           bias_strength = bias_strength,
                           odds_ratio = odds_ratio,
                           heterogeneity = heterogeneity,
                           ma_size = ma_size,
                           stringsAsFactors = F,
                           KEEP.OUT.ATTRS = F)

  # filter out wrong/impossible combinations
  scenarios <- scenarios %>%
                    dplyr::mutate(bias_percentage = ifelse(bias_type == "p", NA, bias_percentage),
                                  bias_strength = ifelse(bias_type == "es", NA, bias_strength)) %>%
                      unique()

  # resetting rownames that got jumbled with the filtering
  rownames(scenarios) <- NULL

  scenarios <- scenarios %>% cbind(scenario_id = 1:nrow(scenarios), .)
}


