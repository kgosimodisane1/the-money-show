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

#Portfolio Return Perfomance

latest_date <- Sys.Date()-1

ret_perf <- rportf[paste(init_date,latest_date, sep = "::")]

cum_ret_perf <- cumprod(1 + ret_perf) -1 #Th e object `ret_perf` is sourced from "Optimisation uing fPortfolio" script.
cum_ret_perf <- (cum_ret_perf + 1) * 100

ggplot(cum_ret_perf) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = INL, col = "INL"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = STX500, col = "STX500"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = STX40, col = "STX40"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = STX100, col = "STX100"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = STXEMG, col = "STXEMG"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = SYGWD, col = "SYGWD"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = PAH3, col = "PAH3"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = CPI, col = "CPI"), linewidth = 0.75) +  
  geom_line(mapping = aes(x = index(cum_ret_perf), y = PPE, col = "PPE"), linewidth = 0.75) +  
  geom_line(mapping = aes(x = index(cum_ret_perf), y = SDO, col = "SDO"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = COH, col = "COH"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = SUI, col = "SUI"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = RIVN, col = "RIVN"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = BTC, col = "BTC"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = ETH, col = "ETH"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(cum_ret_perf), y = UNI, col = "UNI"), linewidth = 0.75) +
  scale_color_manual(values = col, guide = guide_legend(override.aes = list(fill = col))) +
  labs(title = "Return Performance", x = "Time", y = "Return", color = "Legend") +
  theme_dark()

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
  increment <- add_ons
  
  # Fill in the sequence with the growing increment
  for (i in 2:n) {
    sequence[i] <- sequence[i-1] + (sequence[i-1] * growth_rate) + increment
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

# Value at Risk Calculation

Port_VaR <- VaR(na.omit(Return.calculate(Port_perf$Ttl_value)) , method = "gaussian")
Possible_loss <- abs(Port_VaR)*tail(Port_perf$Ttl_value, 1)

# Portfolio return performance 

Port_wght <- as.data.frame(t(apply(Port_perf[,-1], 1, function(row) row / sum(row))))
Port_wght <- rownames_to_column(Port_wght, var = "Date")
rportf.d <- rownames_to_column(as.data.frame(rportf), var = "Date")
merge_ds <- merge(rportf.d, Port_wght, by = "Date", all = FALSE) #try and merge this column
port_ds <- column_to_rownames(merge_ds, var = "Date") #var.x = return, var.y = weight

port_ret <- port_ds$STX500.x*port_ds$STX500.y + port_ds$STX40.x*port_ds$STX40.y +
  port_ds$STX100.x*port_ds$STX100.y + port_ds$STXEMG.x*port_ds$STXEMG.y + 
  port_ds$SYGWD.x*port_ds$SYGWD.y + port_ds$PAH3.x*port_ds$PAH3.y + port_ds$CPI.x*port_ds$CPI.y +
  port_ds$PPE.x*port_ds$PPE.y + port_ds$SDO.x*port_ds$SDO.y + port_ds$COH.x*port_ds$COH.y +
  port_ds$SUI.x*port_ds$SUI.y + port_ds$INL.x*port_ds$INL.y + port_ds$RIVN.x*port_ds$RIVN.y +
  port_ds$BTC.x*port_ds$BTC.y + port_ds$ETH.x*port_ds$ETH.y + port_ds$UNI.x*port_ds$UNI.y


port_ret <- as.data.frame(cbind(merge_ds$Date, port_ret))
port_ret$V1 <- as.Date(port_ret$V1)
port_ret$port_ret <- as.numeric(port_ret$port_ret)
port_ret <- column_to_rownames(as.data.frame(port_ret), var = "V1")
cum_p_ret <- cumprod(1 + port_ret) - 1
cum_p_ret <- as.xts(cum_p_ret)


daily_ret <- prod(1 + port_ret)^(-nrow(port_ret))
annualised_ret <- (1+annualised_ret)^252-1

ggplot(cum_p_ret) +
  geom_line(mapping = aes(x = index(cum_p_ret), y = cum_p_ret$port_ret, col = "red"), linewidth = 0.75) +
  labs(title = "YTD Return Performance", x = "Date", y = "Return") +
  theme_classic()

#Stacked ts graph
# This shows the weight of each asset in the portfolio
                                   
Port_wght_lng_fmt <- Port_wght %>%
  pivot_longer(cols = -Date, names_to = "Stocks", values_to = "Value")

Port_wght_lng_fmt$Date <- as.Date(Port_wght_lng_fmt$Date)

ggplot(Port_wght_lng_fmt, aes(x = Date, y = Value, fill = Stocks)) + 
  geom_area(position = "stack") + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage", "Date", title = "Asset Weight Distribution") + 
  theme_classic()                                   
