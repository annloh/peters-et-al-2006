compute_i_squared <- function(ma_size, var_within, or_sim){
  w <- 1/var_within
  q <- sum(w * (or_sim - sum(w * or_sim)/sum(w))^2)
  i_squared <- ((q-(ma_size-1))/q)
  print(i_squared)
  return(i_squared)
}

i_squared <- sim_data_100 %>% map(~ .x %>% imap( ~{
                            w <- 1/.$var_within
                            q <- sum(w * (log(.$or_sim) - sum(w * log(.$or_sim))/sum(w))^2)
                            i_squared <- max(0,((q-(scenarios$ma_size[.y]-1))/q))
                            }))

i_squared_list <- transpose(i_squared) %>% map(~{unlist(.x)})


i_squared_df <- as_tibble(i_squared,.name_repair = ~ make.names(., unique = TRUE))

real_i2 <- cbind(i_squared_df,heterogeneity = scenarios$heterogeneity)
names(real_i2)

split_list <- real_i2 %>% group_by(heterogeneity) %>% group_split()
boxplot(unlist(split_list[[1]][,1:100]),
unlist(split_list[[2]][,1:100]),
unlist(split_list[[3]][,1:100]),
unlist(split_list[[4]][,1:100]),
names = c("0", "0.2 (0.166)", "1.5 (0.6)", "5 (.83)"),
main = "Intended I^2 vs. observed I^2",
xlab = "Intended I^2",
ylab = "Observed I^2 distribution")
points(1, 0, col="red", pch = 20, cex=2)
points(2, 0.16666, col="red", pch = 20, cex=2)
points(3, 0.6, col = "red", pch = 20, cex=2)
points(4, .83333, col = "red", pch = 20, cex=2)

saveRDS(split_list, "i_squared.rds")
