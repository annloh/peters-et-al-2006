# Helper functions for data generation


# Generate meta-analysis data --------------------------------------------------

#' Generate meta-analysis data.
#'
#' Call all helper functions to generate a dataframe with the simulated data
#' pertaining to a single meta-analysis.
#'
#' @param job_id User defined job_id. Used for determining a unique seed.
#' @param scenario_id Unique id fer scenario = constellation of simulation parameters.
#' @param bias_type Bias type can either be "p" or "es".
#' @param bias_percentage Percentage of studies that will be removed to to publication
#'   bias. Only needs to be provided when bias type = "es".
#' @param bias_strength  String indicating bias severity "moderate" or "strong".
#'   only needs to be supplied when bias type = "p".
#' @param odds_ratio True underlying effect of the symulated meta-analysis.
#' @param heterogeneity Numeric value indicating the heterogeneity fraction.
#' @param ma_size Intended number of studies to be included in final meta-analysis
#'   (after publication bias).
#' @param prob_cg_distr Probablility of an event in the control group.
#'   Can be any value that can be evaluated to a numeric value between 0 and 1.
#'
#' @return A dataframe with ma-size rows. A subset of rows returned by
#'   simulate_unbiased_study_set()
#'


generate_meta_analysis <-function(job_id,
                       scenario_id,
                       bias_type,
                       bias_percentage = NULL,
                       bias_strength = NULL,
                       odds_ratio,
                       heterogeneity,
                       ma_size,
                       prob_cg_distr) {

  set.seed(get_seed(job_id = job_id, scenario_id = scenario_id))

  p_contr <- prob_cg_distr()

  # simulate data for studies in meta-analysis
  ma_data <- simulate_unbiased_study_set(job_id = job_id,
                                         scenario_id = scenario_id,
                                         ma_size = ma_size,
                                         p_contr = p_contr,
                                         odds_ratio = odds_ratio,
                                         bias_type = bias_type,
                                         bias_strength = bias_strength,
                                         bias_percentage = bias_percentage)

  #repeat sampling in case of heterogeneity
  if(heterogeneity > 0){
    tau <- heterogeneity * mean(ma_data$var_within)

    ma_data <- simulate_unbiased_study_set(job_id = job_id,
                                           scenario_id = scenario_id,
                                           ma_size = ma_size,
                                           p_contr = p_contr,
                                           odds_ratio = odds_ratio,
                                           bias_type = bias_type,
                                           bias_strength = bias_strength,
                                           bias_percentage = bias_percentage,
                                           tau = tau)
  }

  ma_data <- apply_publication_bias(ma_data = ma_data,
                                    ma_size = ma_size,
                                    bias_type = bias_type)

  ma_data <- cbind(ma_data, i_squared_biased = compute_i_squared(or_sim = ma_data$or_sim,
                                                                   var_within = ma_data$var_within,
                                                                   k = ma_size))
}



# Simulate single study --------------------------------------------------------

#' Simulate single study.
#'
#'Simulates a single study based on the input parameters.
#'
#' @param job_id user defined job_id.
#' @param scenario_id id of current scenario..
#' @param p_contr Probability of events in control group.
#' @param bias_type Bias type can either be "p" or "es".
#' @param bias_strength  String indicating bias severity "moderate" or "strong".
#'   only needs to be supplied when bias type = "p".
#' @param odds_ratio True underlying effect of the symulated meta-analysis.#'
#' @param tau Product of hereogeneity parameter and mean within study-variance
#'
#' @return A list of descriptives for one simulated study.

simulate_study <- function(job_id,
                      scenario_id,
                      p_contr,
                      bias_type,
                      bias_strength = NULL,
                      odds_ratio,
                      tau = 0){

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

  p_value <- compute_p_value(n = n,
                             event_sim_exp = event_sim_exp,
                             event_sim_contr = event_sim_contr)

  # logical vector of selected studies
  selection <- set_selection_indicator(bias_type, p_value, bias_strength)

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

  # compute additonal study desscriptives that do not depend on sampling
  var_within = 1/a + 1/b + 1/c + 1/d
  se_lnor = sqrt(1/a + 1/b + 1/c + 1/d)
  or_sim = (a*d)/(b*c)

  list(job_id = job_id,
       scenario_id = scenario_id,
       odds_ratio = odds_ratio,
       n = n,
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
       var_within =var_within,
       se_lnor = se_lnor,
       or_sim = or_sim,
       theta = theta)
}



# Simulate full unbiased study set. --------------------------------------------

#' Simulate full unbiased study set.
#'
#' Repeatedly calls \code{simulate_study()} until the intended meta-analysis size equals the
#' number of studies with a positive selection indicator (== 1)
#'
#' @param ma_size passed on to \code{obtain_true_ma_size()}
#' @param p_contr passed on to \code{simulate_study()}
#' @param odds_ratio passed on to \code{simulate_study()}
#' @param bias_type paassed on to \code{simulate_study()} and \code{obtain_true_ma_size()}
#' @param bias_strength passed on to \code{simulate_study()}
#' @param bias_percentage passed on to \code{obtain_true_ma_size()}
#' @param tau passed on to \code{simulate_study()}
#'
#' @return Returns a data frame of all studies pertaining to a given
#'   meta-analysis before publication bias

simulate_unbiased_study_set <- function(job_id,
                                        scenario_id,
                                        ma_size,
                                        p_contr,
                                        odds_ratio,
                                        bias_type,
                                        bias_strength = NULL,
                                        bias_percentage = NULL,
                                        tau = 0){

  # obtain required true number of studies in MA before publication bias
  required_trials <- obtain_true_ma_size(ma_size = ma_size,
                                         bias_type = bias_type,
                                         bias_percentage = bias_percentage)

  # set counter for indexing within the loop
  counter <- 0

  # initiate list for study details to be filled in loop
  ma_data <- list()

  while(required_trials > 0){

    counter <- counter + 1

    ma_data[[counter]] <- simulate_study(job_id = job_id,
                                    scenario_id = scenario_id,
                                    p_contr = p_contr,
                                    odds_ratio = odds_ratio,
                                    tau = tau,
                                    bias_type = bias_type,
                                    bias_strength = bias_strength)

    required_trials <- required_trials -  ma_data[[counter]]$selection
  }

  # transform list to df
  ma_data <- do.call(rbind.data.frame, ma_data)

  # attach original number of studies
  ma_data %<>% mutate(original_k = nrow(ma_data))

  ma_data <- cbind(ma_data, i_squared_unbiased = compute_i_squared(or_sim = ma_data$or_sim,
                    var_within = ma_data$var_within,
                    k = nrow(ma_data)))
}



 # Obtain true number of studies that need to be simulated ---------------------

 #' Obtain true number of studies that need to be simulated.
 #'
 #' @param ma_size Intended number of studies after publication bias.
 #' @param bias_type Bias type can either be "p" or "es".
 #' @param bias_percentage Percentage of studies that will be removed to to
 #'   publication bias. Only needs to be provided when bias type = "es".
 #'
 #' @return Returns a numerical value indicating the adapted sample size

 obtain_true_ma_size <- function(ma_size, bias_type, bias_percentage = NULL){
   switch(bias_type,
          es = round(ma_size/(1 - bias_percentage)),
          p  = ma_size)
 }



 # Compute p_value. ------------------------------------------------------------

#' Compute p_value.
#'
#' Obtains one-sided p-value from chi-square test.
#'
#' @param n size of both exposed and control group.
#' @param event_sim_exp  Simuated numbr of events in exposed group.
#' @param event_sim_contr Simulated number of events in control-group.
#'
#' @return Returns one sided p-value from chi-square test.

compute_p_value <- function(n, event_sim_exp, event_sim_contr){

    matrix(c(event_sim_exp, (n - event_sim_exp),
                      event_sim_contr, (n - event_sim_contr)),
                    nrow = 2, byrow = T) %>%
      chisq.test(correct = TRUE) %>%
        .$p.value/2
}



# Compute selection probablility -----------------------------------------------

#' Compute selection probablility.
#'
#' Function computes selection probability based on p_value and intended bia strength.
#'
#' @param p_value p_value (one tailed)
#' @param bias_strength String indicating the bias strength can be "moderate" or "severe"
#'
#' @return Returns probabilty of publication

select_prob <- function(p_value, bias_strength){
  if (bias_strength == "moderate") {
    p_table <- c(0.05, 0.2, 0.5, 1)
    sec_table <- c(1, 0.75, 0.5, 0.25)
  } else{
    p_table <- c(0.05, 0.2, 1)
    sec_table <- c(1, 0.75, 0.25)
  }
  # output selection probability
  sec_table[min(which(p_table > p_value))]
}



# Set selection idicator. ------------------------------------------------------

#' Set selection idicator.
#'
#' If bias type is "p" one sample will be drawn from a binomial distribution with
#' the probability parameter dertermined by \code{select_prob()} based on bias strength
#' and size of p value.
#'
#' @param bias_type Bias type can either be "p" or "es".
#' @param p_value p_value (one tailed)
#' @param bias_strength String indicating the bias strength can be "moderate" or "severe"
#'  passed to \code{select_prob()}
#'
#' @return 1 if bias type is "es" (selection will then be performed later).
#'         1 if bias type is "p" and the draw from the binomial distribution returned 1.
#'         0 if bias type is "p" and the draw from the binomial distribution returned 1.

set_selection_indicator <- function(bias_type, p_value, bias_strength){
  if(bias_type == "p"){
    p_value %>%  {rbinom(n = 1, size = 1,
                                      prob = select_prob(p_value = .,
                                      bias_strength = bias_strength))}
  } else{1}
}



# Apply publication bias. ------------------------------------------------------

#' Apply publication bias.
#'
#' Function turns unbiased set of studies into biased one (if applicable).
#'
#' @param ma_data Dataset with simulated studies before publication bias.
#' @param bias_type Bias type can either be "p" or "es".
#'
#' @return Returns biased set of simulated study.

apply_publication_bias <- function(ma_data,
                                   ma_size,
                                   bias_type){

  switch(bias_type,
         es = ma_data %>% top_n(ma_size, or_sim),
         p = ma_data %>% filter(selection == 1)
  )
}

# Comupte i_squared. -----------------------------------------------------------

#' Comupte i_squared
#'
#' @param or_sim vector with odds ratios of studies in meta analysis
#' @param var_within vector Within study variances for each study in meta-analysis.
#' @param k number of studies in meta-analysis
#'
#' @return Isquared

compute_i_squared <- function(or_sim,
                              var_within,
                              k){
                            ln_or <- log(or_sim)
                            w <- 1/var_within
                            q <- sum(w * (ln_or - sum(w * ln_or)/sum(w))^2)
                            i_squared <- max(0, ((q-(k- 1))/q))
}


#' Get unique seed.-------------------------------------------------------------
#'
#' @param job_id User generated job_id
#' @param scenario_id id of the current scenario
#'
#' @return seed to be used for the combination of job_id and scenario_id

get_seed <- function(job_id,
                     scenario_id){
  seed <- digest(object = c(job_id, scenario_id),
                 algo="xxhash32") %>%
                    substr(start = 1, stop = 7) %>%
                      strtoi(base = 16)
}
