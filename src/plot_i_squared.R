# i-squared boxplots


# Load scenarios ----------------------------------------------------------
scenarios <- readRDS("scenarios.rds")

# Load simulated data -----------------------------------------------------
sim_data<- readRDS("sim_data.rds")


# Compile plotting data ---------------------------------------------------

ma_level_df <- sim_data %>%
  group_by(job_id, scenario_id) %>%
    summarize(i_squared_unbiased = mean(i_squared_unbiased),
           i_squared_biased = mean(i_squared_biased))

plot_data <-
  left_join(ma_level_df, scenarios, by = "scenario_id") %>%
  mutate(bias_type_fct = factor(bias_type,
                                levels = c("p","es"),
                                labels = c("p-value", "effect size")))


# Plotting I^2 before publication bias ------------------------------------

bloxplot_biased <- ggplot(plot_data, aes(x = factor(heterogeneity),
                                           y = i_squared_biased)) +
  geom_violin(aes(color = bias_type_fct, fill = bias_type_fct),
              alpha = 0.2,
              draw_quantiles = c(0.5)) +
  geom_violin(fill = NA,
              size = 0.7,
              draw_quantiles = c(0.5)) +
  geom_point(data = data.frame(x = factor(c(0, 0.2, 1.5, 5)),
                               y = c(0, 0.16666, 0.6, 0.83)),
             aes(x = x, y = y),
             size = 3,
             color ="red") +
  theme_classic() +
  labs(
    title = "Intended vs observed heterogeneity after publication bias",
    x = "Intended heterogeneity",
    y = "Observed heterogeneity",
    fill = "Bias Type",
    color = "Bias Type"
  )


# Plotting I^2 after publication bias -------------------------------------


bloxplot_unbiased <- ggplot(plot_data, aes(x = factor(heterogeneity),
                                           y = i_squared_unbiased)) +
  geom_violin(aes(color = bias_type_fct, fill = bias_type_fct),
              alpha = 0.2,
              draw_quantiles = c(0.5)) +
  geom_violin(fill = NA,
              size = 0.7,
              draw_quantiles = c(0.5)) +
  geom_point(data = data.frame(x = factor(c(0, 0.2, 1.5, 5)),
                               y = c(0, 0.16666, 0.6, 0.83)),
             aes(x = x, y = y),
             size = 3,
             color ="red") +
  theme_classic() +
  labs(
    title = "Intended vs observed heterogeneity before publication bias",
    x = "Intended heterogeneity",
    y = "Observed heterogeneity",
    fill = "Bias Type",
    color = "Bias Type"
  )


# Combining both plots ----------------------------------------------------

pdf("i_squared_plot.pdf", width=16, height=8)

bloxplot_biased + bloxplot_unbiased +
  plot_layout(guides = 'collect')

dev.off()
