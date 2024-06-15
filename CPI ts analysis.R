library(tidyverse)
library(quantmod)
library(tsDyn)
library(tseries)
library(urca)
library(vars)
library(broom)
library(readxl)

# What is likely to predict the Capitec share price?

# 1. SA Finance Industry (This must be split betw Banking and 
#     General finance/Investment & Insurance.
#     A global banking index can be used as well.)
# 2. U.S. Treasury
# 3. SA Bond prices (we can use a satrix etf as a proxy)
# 4. Overall market indices (JSE top 40, S&P, Nasdaq, and eurstoxx50)
# 5. Real Estate Industry
# 6. Automotive Industry
# 7. Consumer Goods Industry

# SA's publicly listed banks:
# - ABSA
# - Standard Bank
# - FNB "FirstRand Bank"
# - Nedbank 
# - Capitec
# - Investec Bank
# - African Bank

# Pulling stock data for South African banks

getSymbols(c("CPI.JO", "ABG.JO", "SBKP.JO", "FSR.JO", "NED.JO", "INL.JO"), 
           from = Sys.Date()-740, 
           to = Sys.Date()) #listed in ZARcents

# Colleting Live Market Cap data for same stocks -> this is to understand market share

SA_Banks_Mkt_Cap <- read_excel("SA Banks Market Cap.xlsx") #listed in ZARcents
colnames(SA_Banks_Mkt_Cap) <- c("CPI", "ABG", "SBK", "FSR", "NED", "INL")

Ttl_Banks_MktC <- sum(SA_Banks_Mkt_Cap)

SA_Bank_Mkt_Share <- SA_Banks_Mkt_Cap/Ttl_Banks_MktC

Banks_Prices <- cbind(CPI.JO$CPI.JO.Adjusted, ABG.JO$ABG.JO.Adjusted, 
                      SBK.JO$SBK.JO.Adjusted, FSR.JO$FSR.JO.Adjusted, 
                      NED.JO$NED.JO.Adjusted, INL.JO$INL.JO.Adjusted)
colnames(Banks_Prices) <- c("CPI", "ABG", "SBK", "FSR", "NED", "INL")

Banks_ret <- na.omit(
  Return.calculate(
    na.omit(Banks_Prices)
  )
)

# There has to be a cleaner way to do the below code
#Label this as an issue for Git

RetxWeights <- Banks_ret
RetxWeights <- Banks_ret$CPI * SA_Bank_Mkt_Share$CPI
RetxWeights <- Banks_ret$ABG * SA_Bank_Mkt_Share$ABG
RetxWeights <- Banks_ret$SBK * SA_Bank_Mkt_Share$SBK
RetxWeights <- Banks_ret$FSR * SA_Bank_Mkt_Share$FSR
RetxWeights <- Banks_ret$NED * SA_Bank_Mkt_Share$NED
RetxWeights <- Banks_ret$INL * SA_Bank_Mkt_Share$INL

SA_Bank_Index <- Banks_ret$CPI + Banks_ret$ABG + Banks_ret$SBK + 
  Banks_ret$FSR + Banks_ret$NED + Banks_ret$INL
colnames(SA_Bank_Index) <- c("Index")

# Bank Performances

## Cumulative returns

cum_ret <- cumprod(1 + Banks_ret) -1 
indexed_performance <- (cum_ret + 1) * 100

ttl_cum_ret <- cumprod(1 + SA_Bank_Index) -1 
ttl_indexed_performance <- (ttl_cum_ret + 1) * 100

## plot

c <- c("CPI" = "grey", "ABG" = "red", "SBK" = "royalblue", "FSR" = "gold", 
       "NED" = "darkgreen", "INL" = "navy")

ggplot(indexed_performance) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), size = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = ABG, col = "ABG"), size = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = SBK, col = "SBK"), size = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = FSR, col = "FSR"), size = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = NED, col = "NED"), size = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = INL, col = "INL"), size = 0.75) +
  scale_color_manual(values = c, guide = guide_legend(override.aes = list(fill = c))) +
  labs(title = "Return Performance of South African Banks", x = "Time", y = "Return", color = "Legend") +
  theme_classic() #High Correlation is Evident

ggplot(as.data.frame(ttl_indexed_performance)) + 
  geom_line(mapping = aes(x = index(SA_Bank_Index), y = Index), col = "black") + 
  labs(title = "Return Performance for the Banking Industry", x = "Time", y = "Return") +
  theme_classic() #CPI and FSR accounting for 48% of the market has reduced the performance of the banking industry
