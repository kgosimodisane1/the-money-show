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

#### PULLING DATA ####

getSymbols(c("^GSPC", "^DJI", "^NDX", "^VIX", "GC=F", "CL=F", "^TNX", "^TYX", 
             "MWS=F", "MME=F", "DX-Y.NYB", "ZAR=X", "STX500.JO"), 
           from = Sys.Date() - 740,
           to = Sys.Date())

