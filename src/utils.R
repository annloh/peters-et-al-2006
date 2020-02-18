# function to simulate a single meta analysis

generate_ma <- function(job_id, scenario_id, bias_type, bias_percentage,
                        bias_strength, odds_ratio, heterogeneity, ma_size, prob_cg_distr) {
  set.seed(job_id)
  p_contr <- eval(prob_cg_distr) #is this fixed over all studies in one meta-analysis?

  # obtain required true number of studies in MA before publication bias
  required_trials <- obtain_true_ma_size(ma_size = ma_size,
                                         bias_type = bias_type,
                                         bias_percentage = bias_percentage)

  # simulate data for studies in meta-analysis
  ma_data <- simulate_ma_data(required_trials = required_trials,
                              p_contr = p_contr,
                              odds_ratio = odds_ratio,
                              bias_type = bias_type,
                              bias_strength = bias_strength)

  #repeat sampling in case of heterogeneity
  if(heterogeneity > 0){
    tau <- heterogeneity * mean(ma_data$var_within)
    required_trials <- obtain_true_ma_size(ma_size = ma_size,
                                           bias_type = bias_type,
                                           bias_percentage = bias_percentage)

    ma_data <- simulate_ma_data(required_trials = required_trials,
                                p_contr = p_contr,
                                odds_ratio = odds_ratio,
                                bias_type = bias_type,
                                bias_strength = bias_strength,
                                tau = tau)
  }

  # apply publication bias
  ma_biased <- switch(bias_type,
                    es = ma_data %>% top_n(ma_size, or_sim),
                    p = ma_data %>% filter(selection == 1)
                    )

return(ma_biased)
}

# function outputs selection probability based on p_value and bias strength
select_prob <- function(p_value, bias_strength){
  if (strength == "moderate") {
    p_table <- c(0.05, 0.2, 0.5, 1)
    sec_table <- c(1, 0.75, 0.5, 0.25)
  } else{
    p_table <- c(0.05, 0.2, 1)
    sec_table <- c(1, 0.75, 0.25)
  }
# output selection probability
  sec_table[min(which(p_table > p_value))]
}

obtain_true_ma_size <- function(ma_size, bias_type, bias_percentage = NULL){
  switch(bias_type,
  es = ma_size/(1 - bias_percentage),
  p  = ma_size)
}


add_study <- function(p_contr, bias_type, bias_strength = NULL, odds_ratio, tau = 0){

  theta <- rnorm(1, mean = log(odds_ratio), sd = sqrt(tau))

  # odds in exposure and control group
  odds_contr <- p_contr / (1 - p_contr)
  odds_exp <- odds_contr * exp(theta)

  # probability of event in exposure group
  p_exp <- odds_exp/ (1 + odds_exp)

  # n control subjects (page 15 technical report)
  n <- exp(rnorm(1, mean = 5, sd = 0.3)) %>% round()

  sim_contr <- rbinom(n, size = 1, prob = p_contr)
  sim_exp <-  rbinom(n, size = 1, prob = p_exp)

  p_sim_contr <- sim_contr %>% mean
  p_sim_exp <- sim_exp %>% mean

  event_sim_contr <- sim_contr %>% sum
  event_sim_exp <- sim_exp %>% sum


  #computing p-value from chi-square test

  p_value <- matrix(c(event_sim_exp, (n - event_sim_exp),
                      event_sim_contr, (n - event_sim_contr)),
                    nrow = 2, byrow = T) %>%
                      chisq.test(correct = TRUE) %>%
                        .$p.value/2

  # careful Peters used one sided p-values!!!

  # logical vector of selected studies
  if(bias_type == "p"){
    selection <- p_value %>%  {rbinom(n = 1, size = 1,
                                    prob = select_prob(p_value = .,
                                                       bias_strength = bias_strength))}
  } else{selection <- 1}

  # Computing a,b,c,d from 2x2 table
  # a = number of events in exposed group
  # b = number of events in control group
  # c = number of non-events in exposed group
  # d = number of non events in control group

  # add .5 to empty cells
  a <- pmax(event_sim_exp, 0.5)
  b <- pmax(event_sim_contr, 0.5)
  c <- pmax(n - event_sim_exp, 0.5)
  d <- pmax(n - event_sim_contr, 0.5)

  list(n = n,
       p_sim_contr = p_sim_contr,
       p_sim_exp = p_sim_exp,
       event_sim_contr = event_sim_contr,
       event_sim_exp = event_sim_exp,
       p_value = p_value,
       selection = selection,
       a = a,
       b = b,
       c = c,
       d = d,
       theta = theta)
  }

simulate_ma_data <- function(required_trials, p_contr, odds_ratio, bias_type,
                             bias_strength = NULL, tau = 0){

  #initiate list for study details
  counter <- 0
  ma_data <- list()

  while(required_trials > 0){

    counter <- counter + 1

    ma_data[[counter]] <- add_study(p_contr = p_contr,
                                    odds_ratio = odds_ratio,
                                    tau = tau,
                                    bias_type = bias_type,
                                    bias_strength = bias_strength)

    required_trials <- required_trials -  ma_data[[counter]]$selection

  }

  # transform list to df
  ma_data <- do.call(rbind.data.frame, ma_data)


  # compute additonal study desscriptives that do not depend on sampling
  ma_data %>% mutate(var_within = 1/a + 1/b + 1/c + 1/d,
                     se_lnor = sqrt(1/a + 1/b + 1/c + 1/d),
                     or_sim = (a*d)/(b*c))
}


