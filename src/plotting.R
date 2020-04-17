
# Load plotting functions -------------------------------------------------

source("src/dependencies.R")
source("src/plot_error_rate.R")

# Load analysed data ------------------------------------------------------

results_h0_true <- readRDS("results_h0_true.rds")
results_h0_false <- readRDS("results_h0_false.rds")



# Plot type 1 error rate --------------------------------------------------

plot_type_1 <- plot_error_rate(df = results_h0_true,
                               titel = "Type 1 error rate",
                               yaxis = "Type 1 error rate",
                               facet1 = ma_size,
                               facet2 = odds_ratio,
                               facet3 = heterogeneity,
                               facet4 = bias_type)$plots


# Plot power --------------------------------------------------------------

plot_power <- plot_error_rate(df = results_h0_false,
                               titel = "Power",
                               yaxis = "Power",
                               facet1 = ma_size,
                               facet2 = odds_ratio,
                               facet3 = heterogeneity,
                               facet4 = bias_type)$plots



# Combining both plots ----------------------------------------------------

pdf("error_rate_plot.pdf", width=16, height=8)

((plot_type_1[[1]]/plot_type_1[[2]]) | wrap_plots(plot_power))+
  plot_layout(guides = 'collect')

dev.off()

#if labels not visible open with acrobat reader


# # idea for group_by alternative
# library(ggplot2)
# plots <- mtcars %>%
#   split(.$cyl) %>%
#   map(~ggplot(., aes(mpg, wt)) + geom_point())
# paths <- stringr::str_c(names(plots), ".pdf")
#
# pwalk(list(paths, plots), ggsave, path = tempdir())
