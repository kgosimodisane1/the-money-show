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

cb <- c("CPI" = "grey", "Bank Index" = "black")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(as.data.frame(ttl_indexed_performance), mapping = aes(x = index(SA_Bank_Index), y = Index, col = "Bank Index"), linewidth = 0.75) + 
  scale_color_manual(values = cb, guide = guide_legend(override.aes = list(fill = cb))) +
  labs(title = "Capitec vs the Banking Industry", x = "Time", y = "Return", color = "Legend") +
  theme_classic() #CPI and FSR accounting for 48% of the market has reduced the performance of the banking industry

# US TREASURY

getSymbols("^FVX",
           from = Sys.Date()-740, 
           to = Sys.Date())

Treasury <- FVX$FVX.Adjusted/100 #dividing by 100 converts yield to decimals thus reflecting percentages

# SA BOND INDEX

getSymbols("STXGOV.JO", 
           from = Sys.Date()-740, 
           to = Sys.Date())

GovBond <- STXGOV.JO$STXGOV.JO.Adjusted

GovBond_ret <- na.omit(
  Return.calculate(
    na.omit(GovBond)
  )
)

# MARKET INDICES
# JSE Top40, S&P, Nasdaq, and eurstoxx50 and stoxx europe 600

getSymbols(c("^J200.JO", "^GSPC", "^NDX", "^STOXX50E", "^STOXX"),
           from = Sys.Date()-740,
           to = Sys.Date())

M_Indices <- cbind(J200.JO$J200.JO.Adjusted, GSPC$GSPC.Adjusted, NDX$NDX.Adjusted,
                   STOXX50E$STOXX50E.Adjusted, STOXX$STOXX.Adjusted)

M_Ind_ret <- na.omit(
  Return.calculate(
    na.omit(M_Indices)
  )
)

# REAL ESTATE

getSymbols(c("^J805.JO"), 
           from = Sys.Date() - 740,
           to = Sys.Date())

REIT <- J805.JO$J805.JO.Adjusted

REIT_ret <- na.omit(
  Return.calculate(
    na.omit(REIT)
  )
)

SA_RE_Mkt_Cap <- read_excel("SA Banks Market Cap.xlsx", 
                            sheet = "Real Estate")
colnames(SA_RE_Mkt_Cap) <- c("GRT", "RDF", "FFB", "RES", "VKE", "HYP", "ATT", "SSS",
                             "SAC", "EMI", "ACS", "OCT", "SAR", "BWN", "APF", "TEX", 
                             "TMT", "PPR", "DIB")

SA_RE_Mkt_Share <- SA_RE_Mkt_Cap/sum(SA_RE_Mkt_Cap)

getSymbols(c("GRT.JO", "RDF.JO", "FFB.JO", "RES.JO", "VKE.JO", "HYP.JO", "ATT.JO", 
             "SSS.JO", "SAC.JO", "EMI.JO","ACS.JO", "OCT.JO", "SAR.JO", "BWN.JO", 
             "APF.JO", "TEX.JO", "TMT.JO", "PPR.JO", "DIB.JO"),
           from = Sys.Date() - 740,
           to = Sys.Date())

RE_Prices <- cbind(GRT.JO$GRT.JO.Adjusted, RDF.JO$RDF.JO.Adjusted, FFB.JO$FFB.JO.Adjusted,
                   RES.JO$RES.JO.Adjusted, VKE.JO$VKE.JO.Adjusted, HYP.JO$HYP.JO.Adjusted, 
                   ATT.JO$ATT.JO.Adjusted, SSS.JO$SSS.JO.Adjusted, SAC.JO$SAC.JO.Adjusted,
                   EMI.JO$EMI.JO.Adjusted, ACS.JO$ACS.JO.Adjusted, OCT.JO$OCT.JO.Adjusted,
                   SAR.JO$SAR.JO.Adjusted, BWN.JO$BWN.JO.Adjusted, APF.JO$APF.JO.Adjusted, 
                   TEX.JO$TEX.JO.Adjusted, TMT.JO$TMT.JO.Adjusted, PPR.JO$PPR.JO.Adjusted, 
                   DIB.JO$DIB.JO.Adjusted)
colnames(RE_Prices) <- c("GRT", "RDF", "FFB", "RES", "VKE", "HYP", "ATT", "SSS", 
                         "SAC", "EMI", "ACS", "OCT", "SAR", "BWN", "APF", "TEX", 
                         "TMT", "PPR", "DIB")

RE_ret <- na.omit(
  Return.calculate(
    na.omit(RE_Prices)
  )
)

RE_RW <- RE_ret
RE_RW$GRT <- RE_ret$GRT * SA_RE_Mkt_Share$GRT
RE_RW$RDF <- RE_ret$RDF * SA_RE_Mkt_Share$RDF
RE_RW$FFB <- RE_ret$FFB * SA_RE_Mkt_Share$FFB
RE_RW$RES <- RE_ret$RES * SA_RE_Mkt_Share$RES
RE_RW$VKE <- RE_ret$VKE * SA_RE_Mkt_Share$VKE
RE_RW$HYP <- RE_ret$HYP * SA_RE_Mkt_Share$HYP
RE_RW$ATT <- RE_ret$ATT * SA_RE_Mkt_Share$ATT
RE_RW$SSS <- RE_ret$SSS * SA_RE_Mkt_Share$SSS
RE_RW$SAC <- RE_ret$SAC * SA_RE_Mkt_Share$SAC
RE_RW$EMI <- RE_ret$EMI * SA_RE_Mkt_Share$EMI
RE_RW$ACS <- RE_ret$ACS * SA_RE_Mkt_Share$ACS
RE_RW$OCT <- RE_ret$OCT * SA_RE_Mkt_Share$OCT
RE_RW$SAR <- RE_ret$SAR * SA_RE_Mkt_Share$SAR
RE_RW$BWN <- RE_ret$BWN * SA_RE_Mkt_Share$BWN
RE_RW$APF <- RE_ret$APF * SA_RE_Mkt_Share$APF
RE_RW$TEX <- RE_ret$TEX * SA_RE_Mkt_Share$TEX
RE_RW$TMT <- RE_ret$TMT * SA_RE_Mkt_Share$TMT
RE_RW$PPR <- RE_ret$PPR * SA_RE_Mkt_Share$PPR
RE_RW$DIB <- RE_ret$DIB * SA_RE_Mkt_Share$DIB

RE_Index <- RE_RW$GRT + RE_RW$RDF + RE_RW$FFB + RE_RW$RES + RE_RW$VKE + RE_RW$HYP + 
  RE_RW$ATT + RE_RW$SSS + RE_RW$SAC + RE_RW$EMI + RE_RW$ACS + RE_RW$OCT + RE_RW$SAR + 
  RE_RW$BWN + RE_RW$APF + RE_RW$TEX + RE_RW$TMT + RE_RW$PPR + RE_RW$DIB
colnames(RE_Index) <- c("Index")

# AUTOMOBILE INDUSTRY
# Not Applicable due to frequency mismatch
# Vehicle sales would be a great proxy although this info is given monthly

# RETAILERS INDEX

getSymbols("^JS4041.JO", 
           from = Sys.Date()-740,
           to = Sys.Date())

Retail <- JS4041.JO$JS4041.JO.Adjusted

Ret_ret <- na.omit(
  Return.calculate(
    na.omit(Retail)
  )
)

# Clicks Group Limited - CLS
# Dis-Chem Pharmacies Limited - DCP
# Mr Price Group Limited - MRP
# The Foschini Group Limited - TFG
# Truworths International Limited - TRU
# Woolworths Holdings Limited - WHL
# Pick n Pay Stores Limited - PIK
# Shoprite Holdings Limited - SHP
# Spar Group Limited - SPP

SA_Retail_Mkt_Cap <- read_excel("SA Banks Market Cap.xlsx", 
                                sheet = "Retail")
colnames(SA_Retail_Mkt_Cap) <- c("CLS", "DCP", "MRP", "TFG", "TRU", "WHL",
                                 "PIK", "SHP", "SPP")

SA_Retail_Mkt_Share <- SA_Retail_Mkt_Cap/sum(SA_Retail_Mkt_Cap)

getSymbols(c("CLS.JO", "DCP.JO", "MRP.JO", "TFG.JO", "TRU.JO", "WHL.JO", 
             "PIK.JO", "SHP.JO", "SPP.JO"), 
           from = Sys.Date() - 740,
           to = Sys.Date())

Retailers_Prices <- cbind(CLS.JO$CLS.JO.Adjusted, DCP.JO$DCP.JO.Adjusted,
                          MRP.JO$MRP.JO.Adjusted, TFG.JO$TFG.JO.Adjusted,
                          TRU.JO$TRU.JO.Adjusted, WHL.JO$WHL.JO.Adjusted,
                          PIK.JO$PIK.JO.Adjusted, SHP.JO$SHP.JO.Adjusted,
                          SPP.JO$SPP.JO.Adjusted)
colnames(Retailers_Prices) <- c("CLS", "DCP", "MRP", "TFG", "TRU", "WHL",
                                 "PIK", "SHP", "SPP")

Retail_ret <- na.omit(
  Return.calculate(
    na.omit(Retailers_Prices)
  )
)

Retail_RW <- Retail_ret #RW stands for return weights
Retail_RW$CLS <- Retail_ret$CLS * SA_Retail_Mkt_Share$CLS
Retail_RW$DCP <- Retail_ret$DCP * SA_Retail_Mkt_Share$DCP
Retail_RW$MRP <- Retail_ret$MRP * SA_Retail_Mkt_Share$MRP
Retail_RW$TFG <- Retail_ret$TFG * SA_Retail_Mkt_Share$TFG
Retail_RW$TRU <- Retail_ret$TRU * SA_Retail_Mkt_Share$TRU
Retail_RW$WHL <- Retail_ret$WHL * SA_Retail_Mkt_Share$WHL
Retail_RW$PIK <- Retail_ret$PIK * SA_Retail_Mkt_Share$PIK
Retail_RW$SHP <- Retail_ret$SHP * SA_Retail_Mkt_Share$SHP
Retail_RW$SPP <- Retail_ret$SPP * SA_Retail_Mkt_Share$SPP

SA_Retail_Index <- Retail_RW$CLS + Retail_RW$DCP + Retail_RW$MRP + Retail_RW$TFG +
  Retail_RW$TRU + Retail_RW$WHL + Retail_RW$PIK + Retail_RW$SHP + Retail_RW$SPP
colnames(SA_Retail_Index) <- c("Index")
