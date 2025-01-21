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

#### OPTIMAL LAG SELECTION ####

ds_lag_select <- VARselect(fcst_ret, lag.max = 10, type = "const")
ds_lag_select$selection 

GSPC_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$GSPC),
                         lag.max = 10, type = "const")
GSPC_select$selection

DJI_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$DJI),
                         lag.max = 10, type = "const")
DJI_select$selection

NDX_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$NDX),
                         lag.max = 10, type = "const")
NDX_select$selection

VIX_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$VIX),
                         lag.max = 10, type = "const")
VIX_select$selection

Gold_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$Gold),
                         lag.max = 10, type = "const")
Gold_select$selection

Oil_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$Oil),
                         lag.max = 10, type = "const")
Oil_select$selection # 1 & 2 day lag selections

TNX_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$TNX),
                         lag.max = 10, type = "const")
TNX_select$selection

TYX_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$TYX),
                         lag.max = 10, type = "const")
TYX_select$selection

MWS_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$MWS),
                         lag.max = 10, type = "const")
MWS_select$selection

MME_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$MME),
                         lag.max = 10, type = "const")
MME_select$selection

Dollar_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$Dollar),
                         lag.max = 10, type = "const")
Dollar_select$selection

Rand_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$Rand),
                         lag.max = 10, type = "const")
Rand_select$selection # Mostly 2 days lag

STX500_select <- VARselect(cbind(fcst_ret$STX500, fcst_ret$STX500),
                         lag.max = 10, type = "const")
STX500_select$selection

#### FORECASTING ANALYSIS ####

# GSPC

GSPC_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$GSPC), type = "trace", 
                 ecdet = "none", K = 2)
summary(GSPC_jt) # Co-integration is present!!

GSPC_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$GSPC), lag = 1, r =1, 
                  estim = "ML")
summary(GSPC_vecm) # GSPC could be a greater predictor of STX500 performance
# It also has autocorrelation so MAs should be considered as well.

#add irf, causality, FEVD & structural break test

# DJI

DJI_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$DJI), type = "trace",
                ecdet = "none", K = 2)
summary(DJI_jt) # Passed Co-integration test

DJI_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$DJI), lag = 1, r =1, 
                 estim = "ML")
summary(DJI_vecm) # relationship seems to be greater for GSPC 

# NDX

NDX_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$NDX), type = "trace",
                ecdet = "none", K = 2)
summary(NDX_jt) # There is co-integration

NDX_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$NDX), lag = 1, r =1, 
                 estim = "ML")
summary(NDX_vecm) # NDX seems significantly stronger than GSPC

# VIX

VIX_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$VIX), type = "trace",
                ecdet = "none", K =2)
summary(VIX_jt)  # There is co-integration

VIX_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$VIX), lag = 1, r = 1,
               estim = "ML")
summary(VIX_vecm) # Significant relationship

# Gold

Gold_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$Gold), type = "trace",
                 ecdet = "none", K = 2)
summary(Gold_jt)

Gold_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$Gold), lag = 1, r = 1,
                  estim = "ML")
summary(Gold_vecm) # Significant relationship

# Oil

Oil_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$Oil), type = "trace",
                ecdet = "none", K = 3)
summary(Oil_jt)

Oil_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$Oil), lag = 2, r = 1,
                 estim = "ML")
summary(Oil_vecm) # There is statistical significance

# TNX

TNX_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$TNX), type = "trace",
                ecdet = "none", K = 2)
summary(TNX_jt)

TNX_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$TNX), lag = 1, r = 1,
                 estim = "ML")
summary(TNX_vecm) # High significance

# TYX

TYX_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$TYX), type = "trace",
                ecdet = "none", K = 2)
summary(TYX_jt)

TYX_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$TYX), lag = 1, r = 1,
                 estim = "ML")
summary(TYX_vecm)

# MWS

MWS_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$MWS), type = "trace",
                ecdet = "none", K = 2)
summary(MWS_jt)

MWS_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$MWS), lag = 1, r = 1,
                 estim = "ML")
summary(MWS_vecm) # only a significant ect, otherwise weak

# MME

MME_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$MME), type = "trace",
                ecdet = "none", K = 2)
summary(MME_jt)

MME_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$MME), lag = 1, r = 1,
                 estim = "ML")
summary(MME_vecm) # slightly better relationship than MWS

# Dollar

Dollar_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$Dollar), type = "trace",
                   ecdet = "none", K = 2)
summary(Dollar_jt)

Dollar_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$Dollar), lag = 1, r = 1,
                 estim = "ML")
summary(Dollar_vecm) # significant relationship

# Rand 

Rand_jt <- ca.jo(cbind(fcst_ret$STX500, fcst_ret$Rand), type = "trace",
                 ecdet = "none", K = 3)
summary(Rand_jt)

Rand_vecm <- VECM(cbind(fcst_ret$STX500, fcst_ret$Rand), lag = 2, r = 1,
                  estim = "ML")
summary(Rand_vecm) # moderate-weak relationship but still relevant

#### SIMPLE NEURAL NET ####

Vars <- cbind(fcst_ret$STX500, lag(fcst_ret$STX500), lag(lag(fcst_ret$STX500)),
                           lag(fcst_ret$GSPC), lag(fcst_ret$DJI), lag(fcst_ret$NDX), 
                           lag(fcst_ret$VIX), lag(fcst_ret$Gold), lag(fcst_ret$Oil),
                           lag(lag(fcst_ret$Oil)), lag(fcst_ret$TNX), lag(fcst_ret$TYX), 
                           lag(fcst_ret$MWS), lag(fcst_ret$MME), lag(fcst_ret$Dollar), 
                           lag(fcst_ret$Rand), lag(lag(fcst_ret$Rand)))
colnames(Vars) <- c("STX500", "STX500.l1", "STX500.l2", "GSPC.l1", "DJI.l1",
                            "NDX.l1", "VIX.l1", "Gold.l1", "Oil.l1", "Oil.l2", 
                            "TNX.l1", "TYX.l1", "MWS.l1", "MME.l1", "Dollar.l1",
                            "Rand.l1", "Rand.l2")

Vars <- na.omit(Vars)

Train_start <- index(head(Vars, 1))
Test_end <- index(tail(Vars, 1))
Train_end <- Day1 + 0.7 * (index(tail(Vars, 1)) - index(head(Vars, 1)))
Test_start <- Day1_end + 1

Training_set <- Vars[paste(Train_start, Train_end, sep = "::"), ]
Test_set <- Vars[paste(Test_start, Test_end, sep = "::"), ]

test_nnet <- neuralnet(STX500 ~ STX500.l1 + STX500.l2 + GSPC.l1 + DJI.l1 + NDX.l1
                       + VIX.l1 + Gold.l1 + Oil.l1 + Oil.l2 + TNX.l1 + TYX.l1 + 
                         MWS.l1 + MME.l1 + Dollar.l1 + Rand.l1 + Rand.l2, 
                       data = Training_set, hidden = c(12, 6, 3), rep = 10)

plot(test_nnet)

test_pred <- as.xts(predict(test_nnet, Test_set))
colnames(test_pred) <- c("STX500e")
index(test_pred) <- as.Date(index(test_pred)) + 1

ggplot() +
  geom_line(data = Test_set, mapping = aes(x = index(Test_set), y = STX500), col = "black", linewidth = 0.75) + 
  geom_line(data = test_pred, mapping = aes(x = index(test_pred), y = STX500e), col = "red", linewidth = 0.75) +
  theme_classic()

# Testing Predictive Power

data_v_pred <- lm(Test_set$STX500 ~ test_pred$STX500e)
data_v_pred_sum <- summary(data_v_pred)
data_v_pred_sum

model_r2 <- summary(data_v_pred)$r.squared
model_adj.r2 <- data_v_pred_sum$adj.r.squared
model_fs <- data_v_pred_sum$fstatistic

model_est <- data_v_pred_sum$coefficients
model_corr <- model_est[2, 1]
model_pv <- model_est[2, 4]

model_corr <- cor(Test_set$STX500, test_pred$STX500e)


# with a corr = 0.42 and a pv = 0.048, we can say that there is predictive power
# but it is only moderately strong

# EE has a brokerage fee of 0.25% meaning we need to expect a return of higher than
# 0.5% to realize a profit. 
# Thus our model must have a value that is higher than 0.5% > x*0.42 OR 0.005 > x*0.42
# -> 1.19% OR 0.0119 > x

# When including standard error of 0.21 we get 0.5% > x*(0.42 - 0.21) (21)
# -> 2.38% OR 0.0238 > x 

# Should be 0.005 > x*0.42 +/- 0.21

#### BACKTEST ####

# Signals

signal2 <- ifelse(test_pred$STX500e > 0, 1, 
                 ifelse(test_pred$STX500e < 0, -1, 0)
                 )
colnames(signal) <- c("signal")

action1 <- ifelse(signal$signal == 1, "BUY", 
                  ifelse(signal$signal == -1, "SELL", "HOLD"))
colnames(action1) <- c("action1")

action2 <- ifelse(action1$action1 == "BUY" & lag(action1$action1) == "SELL", "BUY",
                  ifelse(action1$action1 == "SELL" & lag(action1$action1) == "BUY", "SELL",
                         "HOLD"))
colnames(action2) <- c("action2")

signal2 <- ifelse(action2$action2 == "BUY", 1, 
                  ifelse(action2$action2 == "SELL", -1, NA))
colnames(signal2) <- c("signal2")

signals_df <- cbind(as.data.frame(signal), as.data.frame(action1), 
                    as.data.frame(action2), as.data.frame(signal2))

# We still need to include transaction costs @ 0.025%

# performance

p_signal <- ifelse(test_pred$STX500e > 0, 1, 0)
colnames(p_signal) <- c("pos_signal")
  
  

pos <- Test_set$STX500*p_signal
colnames(pos) <- c("position")

backtest_perf <- cbind(Test_set$STX500, p_signal, pos)

cum_bt_perf <- cumprod(1 + backtest_perf$position) - 1
cum_STX_perf <- cumprod(1 + Test_set$STX500) - 1

ggplot() +
  geom_line(data = cum_STX_perf, mapping = aes(x = index(cum_STX_perf), y = STX500, col = "STX500"), linewidth = 0.75) + 
  geom_line(data = cum_bt_perf, mapping = aes(x = index(cum_bt_perf), y = position, col = "Neural Net"), linewidth = 0.75) +
  scale_color_manual(values = c("STX500" = "black", "Neural Net" = "red")) +
  labs(title = "Neural Net vs Buy and Hold (exclusive of transaction costs)", x = "Time", y = "Cumulative Return") +
  theme_classic()

# It is still relatively successful

#### BACKTEST incl. TX COSTS ####

tx_cost <- 0.0025

pos_tx <- ifelse(backtest_perf$pos_signal != lag(backtest_perf$pos_signal),
                        backtest_perf$position - tx_cost,
                        backtest_perf)
colnames(pos_tx) <- c("pos_tx")

pos_tx[1, 1] <- ifelse(backtest_perf$pos_signal[1, 1] == 1, 
                      backtest_perf$position[1, 1] - tx_cost, 0)

backtest_perf <- cbind(backtest_perf, pos_tx)

cum_bt_perf_tx <- cumprod(1 + backtest_perf$pos_tx) - 1

ggplot() +
  geom_line(data = cum_STX_perf, mapping = aes(x = index(cum_STX_perf), y = STX500, col = "STX500"), linewidth = 0.75) + 
  geom_line(data = cum_bt_perf_tx, mapping = aes(x = index(cum_bt_perf_tx), y = pos_tx, col = "Neural Net"), linewidth = 0.75) +
  scale_color_manual(values = c("STX500" = "black", "Neural Net" = "red")) +
  labs(title = "Neural Net vs Buy and Hold (including transaction costs)", x = "Time", y = "Cumulative Return") +
  theme_classic()
