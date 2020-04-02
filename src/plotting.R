#plotting function for type one error and power

plot_error_rate <-  function(df,
                             method,
                             titel = NULL,
                             yaxis = NULL,
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
                               size = 1) +
                    labs(title = titel) +
                    ylab(yaxis)
}

plot_error_rate(df = case_wise_type_1_error_both,
            method = "error_rate_alt",
            titel = "Type 1 error rate: Alternative regression test",
            yaxis = "Type 1 error rate",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

plot_error_rate(df = case_wise_type_1_error_both,
            method = "error_rate",
            titel = "Type 1 error rate: Egger's regression test",
            yaxis = "Type 1 error rate",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

plot_error_rate(df = case_wise_power_both,
            method = "error_rate_alt",
            titel = "Power: Alternative regression test",
            yaxis = "Power",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")


plot_error_rate(df = case_wise_power_both,
            method = "error_rate",
            titel = "Power: Egger's regression test",
            yaxis = "Power",
            facet1 = "ma_size",
            facet2 = "odds_ratio",
            facet3 = "heterogeneity",
            facet4 = "bias_type")

# Filter for result tabling

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

foo <- filter_error_rate(df = case_wise_type_1_error_both,
                  facet1 = "ma_size",
                  facet2 = "heterogeneity",
                  facet3 = "odds_ratio",
                  facet4 = "bias_type")

filter_error_rate(df = case_wise_power_both,
                  facet1 = "ma_size",
                  facet2 = "heterogeneity",
                  facet3 = "odds_ratio",
                  facet4 = "bias_type")

#filter more stuff
            filter(ma_size == "6" & heterogeneity %in% c("0","0.2")) %>%
