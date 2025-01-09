library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tsDyn)
library(tseries)
library(urca)
library(vars)
library(broom)
library(readxl)
library(rugarch)
library(neuralnet)
library(keras)

# What is likely to predict tomorrow's Satrix S&P500 stock price?

# S&P 500
# Gold
# Crude Oil
# U.S. Treasury (we can have for different durations)
# Nasdaq
# MSCI World Index (Not readily available/can use MWS=F as a proxy)
# Emerging Markets Index (Not readily available/can use MME=F as a proxy)
# Dollar/Rand Currency
# CBOE Market Volatility Index (VIX)
# We can later add momentum models

#### DATA SET ####

getSymbols(c("^GSPC", "^DJI", "^NDX", "^VIX", "GC=F", "CL=F", "^TNX", "^TYX", 
             "MWS=F", "MME=F", "DX-Y.NYB", "ZAR=X", "STX500.JO"), 
           from = Sys.Date() - 740,
           to = Sys.Date())

fcst_ds <- cbind(GSPC$GSPC.Adjusted, DJI$DJI.Adjusted, NDX$NDX.Adjusted,
                 VIX$VIX.Adjusted, `GC=F`$`GC=F.Adjusted`, `CL=F`$`CL=F.Adjusted`,
                 TNX$TNX.Adjusted, TYX$TYX.Adjusted, `MWS=F`$`MWS=F.Adjusted`, 
                 `MME=F`$`MME=F.Adjusted`, `DX-Y.NYB`$`DX-Y.NYB.Adjusted`,
                 `ZAR=X`$`ZAR=X.Adjusted`, STX500.JO$STX500.JO.Adjusted)

colnames(fcst_ds) <- c("GSPC", "DJI", "NDX", "VIX", "Gold", "Oil", "TNX", "TYX",
                       "MWS", "MME", "Dollar", "Rand", "STX500")

fcst_prices <- na.omit(fcst_ds)

fcst_ret <- na.omit(
  Return.calculate(fcst_prices)
)

#### PLOTS ####

# Indexed Performance

fcst_index <- cumprod(1+fcst_ret)*100

fc <- c("GSPC" = "red", "DJI" = "royalblue", "NDX" = "lightblue", "VIX" = "grey", 
        "Gold" = "gold", "Oil" = "black", "TNX" = "brown", "TYX" = "pink",
        "MWS" = "blue", "MME" = "lightgreen", "Dollar" = "green", "Rand" = "orange",
        "STX500" = "purple")

ggplot(fcst_index) + 
  geom_line(mapping = aes(x = index(fcst_index), y = GSPC, col = "GSPC"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = DJI, col = "DJI"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = NDX, col = "NDX"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = VIX, col = "VIX"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = Gold, col = "Gold"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = Oil, col = "Oil"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = TNX, col = "TNX"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = TYX, col = "TYX"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = MWS, col = "MWS"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = MME, col = "MME"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = Dollar, col = "Dollar"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = Rand, col = "Rand"), linewidth = 0.75) +
  geom_line(mapping = aes(x = index(fcst_index), y = STX500, col = "STX500"), linewidth = 0.75) +
  scale_color_manual(values = fc, guide = guide_legend(override.aes = list(fill = fc))) +
  labs(title = "Asset Return Perfomances", x = "Time", y = "Indexed Return", color = "Legend") +
  theme_classic()

# vGSPC

ggplot(fcst_ret, mapping = aes(x = STX500, y = GSPC)) + 
  geom_point(col = "red", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs S&P 500 Index", x = "STX500 Returns", y = "S&P 500 Returns") +
  theme_classic()

# vDJI

ggplot(fcst_ret, mapping = aes(x = STX500, y = DJI)) + 
  geom_point(col = "royalblue", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "black") +
  labs(title = "Satrix 500 ETF vs Dow Jones Insdustrial Index", x = "STX500 Returns", y = "DJI Returns") +
  theme_classic()

# vNDX

ggplot(fcst_ret, mapping = aes(x = STX500, y = NDX)) + 
  geom_point(col = "lightblue", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "black") +
  labs(title = "Satrix 500 ETF vs Nasdaq 100", x = "STX500 Returns", y = "Nasdaq 100 Returns") +
  theme_classic()

# vVIX

ggplot(fcst_ret, mapping = aes(x = STX500, y = VIX)) + 
  geom_point(col = "grey", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs CBOE Volatility Index", x = "STX500 Returns", y = "VIX Returns") +
  theme_classic()

# vGold

ggplot(fcst_ret, mapping = aes(x = STX500, y = Gold)) + 
  geom_point(col = "gold", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs Gold", x = "STX500 Returns", y = "Gold") +
  theme_classic()

# vOil

ggplot(fcst_ret, mapping = aes(x = STX500, y = Oil)) + 
  geom_point(col = "black", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs Oil", x = "STX500 Returns", y = "Oil Returns") +
  theme_classic()

# vTNX

ggplot(fcst_ret, mapping = aes(x = STX500, y = TNX)) + 
  geom_point(col = "brown", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs 10yr U.S. Treasury Yield", x = "STX500 Returns", y = "Treasury Returns") +
  theme_classic()

# vTYX

ggplot(fcst_ret, mapping = aes(x = STX500, y = TYX)) + 
  geom_point(col = "pink", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs 30yr U.S. Treasury Yield", x = "STX500 Returns", y = "Treasury Returns") +
  theme_classic()

# vMWS

ggplot(fcst_ret, mapping = aes(x = STX500, y = MWS)) + 
  geom_point(col = "blue", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "black") +
  labs(title = "Satrix 500 ETF vs MSCI World Index ETF", x = "STX500 Returns", y = "World Index Returns") +
  theme_classic()

# vMME

ggplot(fcst_ret, mapping = aes(x = STX500, y = MME)) + 
  geom_point(col = "lightgreen", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs MSCI Emerging Markets ETF", x = "STX500 Returns", y = "Emerging Markets Returns") +
  theme_classic()

# vDollar

ggplot(fcst_ret, mapping = aes(x = STX500, y = Dollar)) + 
  geom_point(col = "green", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs US Dollar Index", x = "STX500 Returns", y = "Dollar Returns") +
  theme_classic()

# vRand

ggplot(fcst_ret, mapping = aes(x = STX500, y = Rand)) + 
  geom_point(col = "orange", size = 2) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Satrix 500 ETF vs Rand/Dollar", x = "STX500 Returns", y = "Rand Returns") +
  theme_classic()
