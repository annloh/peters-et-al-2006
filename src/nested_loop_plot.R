indexed_power <- cbind(row_index = 1:nrow(power_table), power_table)

nested_plot <- ggplot(data = indexed_power, aes(x=row_index, y= power,
                                                color = factor(odds_ratio),
                                                shape = bias_type,
                                                size = ma_size)) +
  geom_point()+
  facet_grid(cols = vars(test_type))
names(indexed_power)
