
# Plotting function -------------------------------------------------------

#' Plotting the error rate with various facets
#'
#' @param df data frame with simulation parameters and test results
#' @param titel optinal plot title
#' @param yaxis optional y-axis title
#' @param facet1 first grouping variable
#' @param facet2 second grouping variable
#' @param facet3 third grouping variable
#' @param facet4 fourth grouping variable
#'
#' @return returns a ggplot2 object


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

  data <- df %>%
    group_by(test_type, !!facet1, !!facet2, !!facet3, !!facet4) %>%
      summarise(error_rate = mean(error_rate)) %>% ungroup()

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
