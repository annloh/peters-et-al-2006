# results_h0_true <- readRDS("results_h0_true.rds")
# results_h0_false <- readRDS("results_h0_false.rds")

plot_error_rate <-  function(df,
                             titel = NULL,
                             yaxis = NULL,
                             facet1 = NULL,
                             facet2 = NULL,
                             facet3 = NULL,
                             facet4 = NULL){
                  facet1 <- enquo(facet1)
                  facet2 <- enquo(facet2)
                  facet3 <- enquo(facet3)
                  facet4 <- enquo(facet4)
                                   print(str(df))
                  print(names(df))
                  data <- df %>%
                            group_by(test_type, !!facet1, !!facet2, !!facet3, !!facet4) %>%
                              summarise(error_rate = mean(error_rate)) %>% ungroup()
                                    print(glimpse(data))
                  print(names(data))
                  print(summary(data))
                  print(str(data))
                  data %>% group_by(test_type) %>%
                    do( #plots groupwise (i.e. by test_type)
                      plots=
                        ggplot(data=., aes(x = !!facet1,
                                        y = error_rate,
                              group = factor(!!facet2),
                              color = factor(!!facet2),
                              shape = factor(!!facet2))) +
                              facet_grid(cols = vars(!!facet3),
                                         rows = vars(!!facet4)) +
                              geom_point(size=3)+
                              geom_line(size = 1) +
                             geom_hline(yintercept = 0.1,
                                         linetype = "dashed",
                                         color = "black",
                                         size = 1) +
                              labs(title = titel) +
                              ylab(yaxis)+
                        ggtitle(paste(titel,.$test_type))
                    )
}

plot_error_rate(df = results_h0_true,
            titel = "Type 1 error rate",
            yaxis = "Type 1 error rate",
            facet1 = ma_size,
            facet2 = odds_ratio,
            facet3 = heterogeneity,
            facet4 = bias_type)$plots

plot_error_rate(df = results_h0_false,
            titel = "Power",
            yaxis = "Power",
            facet1 = ma_size,
            facet2 = odds_ratio,
            facet3 = heterogeneity,
            facet4 = bias_type)$plots
