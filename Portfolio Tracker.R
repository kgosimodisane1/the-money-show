library(tidyverse)

cPort_perf <- cbind(Ttl_value, stock_values)

#Port_perf <- rbind(Port_perf, cPort_perf)

if (index(cPort_perf) == tail(index(Port_perf), 1)) {
  Port_perf <- Port_perf
} else {
  Port_perf <- rbind(Port_perf, cPort_perf)
}

col <- c("Ttl_value" = "black", "STX500" = "red", "STX40" = "darkgreen", "STX100" = "grey",
         "STXEMG" = "green", "SYGWD" = "brown", "PAH3" = "orange", "CPI" = "lavender",
         "PPE" = "purple", "SDO" = "yellow", "COH" = "lightblue", "SUI" = "beige", 
         "RIVN" = "blue", "BTC" = "gold", "ETH" = "navy", "UNI" = "pink")

ggplot(Port_perf) +
  geom_line(mapping = aes(x = index(Port_perf), y = Ttl_value, col = "Ttl_value"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = STX500, col = "STX500"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = STX40, col = "STX40"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = STX100, col = "STX100"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = STXEMG, col = "STXEMG"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = SYGWD, col = "SYGWD"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = PAH3, col = "PAH3"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = CPI, col = "CPI"), linewidth = 0.75) +  
  geom_line(mapping = aes(x = index(Port_perf), y = PPE, col = "PPE"), linewidth = 0.75) +  
  geom_line(mapping = aes(x = index(Port_perf), y = SDO, col = "SDO"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = COH, col = "COH"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = SUI, col = "SUI"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = RIVN, col = "RIVN"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = BTC, col = "BTC"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = ETH, col = "ETH"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(Port_perf), y = UNI, col = "UNI"), linewidth = 0.75) +
  scale_color_manual(values = col, guide = guide_legend(override.aes = list(fill = col))) +
  labs(title = "Investment Performance", x = "Time", y = "Rand Value", color = "Legend") +
  theme_classic()

# Add a forecast to the graph

#Principal = 17000
#Annuity = 4000
#Exp.Return = 0.25

# init_date <- Sys.Date() - 3, -> "2024-06-25"
Days <- seq(from = init_date, to = Sys.Date() - 2, by = "day")

obs <- as.numeric(Sys.Date() - init_date - 1)

Principal <-  17000
Annuity <-  4000/30 #monthly annuity divided by days in a month
Exp_ret <-  0.15/252

create_sequence <- function(base, growth_rate, n, add_ons) {
  # Initialize the sequence vector with the base value
  sequence <- numeric(n)
  sequence[1] <- base
  
  # Initial increment
  increment <- 0
  
  # Fill in the sequence with the growing increment
  for (i in 2:n) {
    sequence[i] <- sequence[i-1] + (sequence[i-1] * growth_rate) + increment
    increment <- increment + add_ons
  }
  
  return(sequence)
}

Exp_perf <- create_sequence(Principal, Exp_ret, obs, Annuity)
#colnames(Exp_perf) <- c("Benchmark")

Exp_perf_ts <- data.frame(Days, Exp_perf)

col2 <- c("Portfolio" = "black", "Benchmark" = "turquoise")

ggplot() +
  geom_line(data = Port_perf, mapping = aes(x = index(Port_perf), y = Ttl_value, col = "Portfolio"), linewidth = 0.75) +
  geom_line(data = Exp_perf_ts, mapping = aes(x = Days, y = Exp_perf, col = "Benchmark"), linewidth = 0.75) +
  scale_color_manual(values = col2, guide = guide_legend(override.aes = list(fill = col2))) +
  labs(title = "Portfolio Performance vs Benchmark", x = "Time", y = "Rand Value", color = "Legend") +
  theme_classic()
