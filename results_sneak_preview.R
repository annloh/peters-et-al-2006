# View results


# load dependencies

library(ggplot2)
library(dplyr)

# load results

case_wise_type_1_error_both <- readRDS("case_wise_type_1_error_both.rds")
case_wise_power_both <- readRDS("case_wise_power_both.rds")

i_squared <- readRDS("i_squared.rds")

# overview of variables
# glimpse(case_wise_power_both[,-(1:50)])


# plotting function

plot_error_rate <-  function(df,
                             method,
                             facet1 = NULL,
                             facet2 = NULL,
                             facet3 = NULL,
                             facet4 = NULL){
                  data <- df %>%
                            group_by_at(c(facet1, facet2, facet3, facet4)) %>%
                              summarise(error_rate = mean(!! sym(method)))
                  data %>%
                  ggplot( aes_string(x = paste0("factor(",facet1,")"),
                              y = "error_rate",
                              group = paste0("factor(",facet2,")"),
                              color = paste0("factor(",facet2,")"),
                              shape = paste0("factor(",facet2,")"))) +
                    facet_grid(cols = vars(!! sym(facet3)), rows = vars(!! sym(facet4))) +
                    geom_point(size=3)+
                    geom_line() +
                    geom_hline(yintercept = 0.1,
                               linetype = "dashed",
                               color = "black",
                               size = 1)
}


# some example plots (feel free to change what you want to see here)

plot_error_rate(df = case_wise_type_1_error_both,
            method = "error_rate_alt",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

plot_error_rate(df = case_wise_type_1_error_both,
            method = "error_rate",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

plot_error_rate(df = case_wise_power_both,
            method = "error_rate_alt",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")


plot_error_rate(df = case_wise_power_both,
            method = "error_rate",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

# table filter function

filter_error_rate <- function(df,
                              facet1 = NULL,
                              facet2 = NULL,
                              facet3 = NULL,
                              facet4 = NULL){
  data <- df %>%
              group_by_at(c("ma_size", facet1, facet2, facet3, facet4)) %>%
                summarise(error_rate = mean(error_rate),
                          error_rate_alt = mean(error_rate_alt))
  data
}

# some example tables (feel free to change what you want to see here)
filter_error_rate(df = case_wise_type_1_error_both,
                  facet1 = "ma_size",
                  facet2 = "heterogeneity",
                  facet3 = "odds_ratio",
                  facet4 = "bias_type")

filter_error_rate(df = case_wise_power_both,
                  facet1 = "ma_size",
                  facet2 = "heterogeneity",
                  facet3 = "odds_ratio",
                  facet4 = "bias_type")

# quick and dirty i^2 plot
# (grouped by the intended heterogeneity)

boxplot(unlist(i_squared[[1]][,1:100]),
        unlist(i_squared[[2]][,1:100]),
        unlist(i_squared[[3]][,1:100]),
        unlist(i_squared[[4]][,1:100]),
        names = c("0", "0.2 (0.166)", "1.5 (0.6)", "5 (.83)"))
