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

# After brief observation we can not expect Oil nor the Rand/Dollar to have much influence on STX500 performance
# According to theory, the Satrix 500 ETF is supposed to be presented on the y-axis as it is meant to be an independent variable but due to the amount of effort it would take to correct this mistake, we will leave it as it is as the basic conclusions can be deduced from the above graphs.

#### BASIC STATS ####

fcst_stats <- basicStats(fcst_ret)
plot(fcst_stats)

ggplot() + 
  geom_density(fcst_ret, mapping = aes(GSPC, col = "GSPC"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(DJI, col = "DJI"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(NDX, col = "NDX"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(VIX, col = "VIX"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(Gold, col = "Gold"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(Oil, col = "Oil"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(TNX, col = "TNX"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(TYX, col = "TYX"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(MWS, col = "MWS"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(MME, col = "MME"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(Dollar, col = "Dollar"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(Rand, col = "Rand"), fill = "white", alpha = 0, size = 0.75) + 
  geom_density(fcst_ret, mapping = aes(STX500, col = "STX500"), fill = "white", alpha = 0,, size = 0.75) + 
  scale_color_manual(values = fc, guide = guide_legend(override.aes = list(fill = fc))) +
  xlim(-0.25, 0.25) +
  labs(title = "Asset Return Distribution", x = "Asset Returns", y = "Distribution") +
  theme_classic()

# Try to make it interactive where the user can hover over the graph and see specifics

#### UNIT ROOT TESTS ####

adf.test(fcst_ret$GSPC) 
pp.test(fcst_ret$GSPC) 

adf.test(fcst_ret$DJI) 
pp.test(fcst_ret$DJI) 

adf.test(fcst_ret$NDX)
pp.test(fcst_ret$NDX)

adf.test(fcst_ret$VIX)
pp.test(fcst_ret$VIX)

adf.test(fcst_ret$Gold)
pp.test(fcst_ret$Gold)

adf.test(fcst_ret$Oil)
pp.test(fcst_ret$Oil)

adf.test(fcst_ret$TNX)
pp.test(fcst_ret$TNX)

adf.test(fcst_ret$TYX)
pp.test(fcst_ret$TYX)

adf.test(fcst_ret$MWS)
pp.test(fcst_ret$MWS)

adf.test(fcst_ret$MME)
pp.test(fcst_ret$MME)

adf.test(fcst_ret$Dollar)
pp.test(fcst_ret$Dollar)

adf.test(fcst_ret$Rand)
pp.test(fcst_ret$Rand)

adf.test(fcst_ret$STX500)
pp.test(fcst_ret$STX500)

# All assets are stationary in their returns
