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

#### PULLING DATA ####

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

#### PLOTS AND BASIC STATS ####

## Bank Performances

### Cumulative returns

cum_ret <- cumprod(1 + Banks_ret) -1 
indexed_performance <- (cum_ret + 1) * 100

ttl_cum_ret <- cumprod(1 + SA_Bank_Index) -1 
ttl_indexed_performance <- (ttl_cum_ret + 1) * 100

### Plotting banks preformance

c <- c("CPI" = "grey", "ABG" = "red", "SBK" = "royalblue", "FSR" = "gold", 
       "NED" = "darkgreen", "INL" = "navy")

ggplot(indexed_performance) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = ABG, col = "ABG"), linewidth = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = SBK, col = "SBK"), linewidth = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = FSR, col = "FSR"), linewidth = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = NED, col = "NED"), linewidth = 0.75) + 
  geom_line(mapping = aes(x = index(indexed_performance), y = INL, col = "INL"), linewidth = 0.75) +
  scale_color_manual(values = c, guide = guide_legend(override.aes = list(fill = c))) +
  labs(title = "Return Performance of South African Banks", x = "Time", y = "Return", color = "Legend") +
  theme_classic() #High Correlation is Evident

### Plotting CPI against Banking Industry

cb <- c("CPI" = "grey", "Bank Index" = "black")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(as.data.frame(ttl_indexed_performance), mapping = aes(x = index(SA_Bank_Index), y = Index, col = "Bank Index"), linewidth = 0.75) + 
  scale_color_manual(values = cb, guide = guide_legend(override.aes = list(fill = cb))) +
  labs(title = "Capitec vs the Banking Industry", x = "Time", y = "Return", color = "Legend") +
  theme_classic() #CPI and FSR accounting for 48% of the market has reduced the performance of the banking industry

Banks_plot <- merge(Banks_ret$CPI, SA_Bank_Index)

ggplot(as.data.frame(Banks_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "black", size = 2) + 
  scale_x_continuous(limits = c(-0.5, 0.25)) +
  scale_y_continuous(limits = c(-0.5, 0.25)) +
  geom_smooth(method=lm, se=FALSE, col = "red") +
  labs(title = "Capitec & SA Banks Correlation", x = "Capitec Returns", y = "SA Bank Returns") + 
  theme_classic() #line of best fit is skewed because of outliers 

ggplot(as.data.frame(Banks_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "black", size = 2) + 
  scale_x_continuous(limits = c(-0.25, 0.25)) +
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  geom_smooth(method=lm, se=FALSE, col = "red") +
  labs(title = "Capitec & SA Banks Correlation", x = "Capitec Returns", y = "SA Bank Returns") + 
  theme_classic() #line of best fit is skewed because of outliers 

### CPI vs U.S. Treasury

ust_cum_ret <- cumprod(1 + Treasury) -1 
ust_indexed_performance <- (ust_cum_ret + 1) * 100

ct <- c("CPI" = "grey", "U.S. Treasury" = "royalblue")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(ust_indexed_performance, mapping = aes(x = index(ust_indexed_performance), y  = FVX.Adjusted, col = "U.S. Treasury"), linewidth = 0.75) +
  scale_color_manual(values = ct, guide = guide_legend(override.aes = list(fill = ct))) +
  labs(title = "Capitec vs U.S. Treasury", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 
# Another issue. we are struggling to correctly plot the us treasury
# use: PV = FV/(1+r)^t 
# where: FV = 100 and t = 5

ggplot() + 
  geom_line(Banks_ret, mapping = aes(x = index(Banks_ret), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(Treasury, mapping = aes(x = index(Treasury), y  = FVX.Adjusted, col = "U.S. Treasury"), linewidth = 0.75) +
  scale_color_manual(values = ct, guide = guide_legend(override.aes = list(fill = ct))) +
  labs(title = "Capitec vs U.S. Treasury", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 
# First idea looks promising

### CPI vs SA Bond ETF

gb_cum_ret <- cumprod(1 + GovBond_ret) -1 
gb_indexed_performance <- (gb_cum_ret + 1) * 100

cgb <- c("CPI" = "grey", "Gov. Bonds" = "darkgreen")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(gb_indexed_performance, mapping = aes(x = index(gb_indexed_performance), y  = STXGOV.JO.Adjusted, col = "Gov. Bonds"), linewidth = 0.75) +
  scale_color_manual(values = cgb, guide = guide_legend(override.aes = list(fill = cgb))) +
  labs(title = "Capitec vs SA Gov. Bonds", x = "Time", y = "Return", color = "Legend") +
  theme_classic() # Correlation is present; CPI seems to exhibit more volatility


### CPI vs SA Bond ETF

gb_cum_ret <- cumprod(1 + GovBond_ret) -1 
gb_indexed_performance <- (gb_cum_ret + 1) * 100

cgb <- c("CPI" = "grey", "Gov. Bonds" = "darkgreen")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(gb_indexed_performance, mapping = aes(x = index(gb_indexed_performance), y  = STXGOV.JO.Adjusted, col = "Gov. Bonds"), linewidth = 0.75) +
  scale_color_manual(values = cgb, guide = guide_legend(override.aes = list(fill = cgb))) +
  labs(title = "Capitec vs SA Gov. Bonds", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 

GovBond_plot <- merge(Banks_ret$CPI, GovBond_ret)

ggplot(as.data.frame(GovBond_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "darkgreen", size = 2) + 
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  geom_smooth(method=lm, se=FALSE, col = "orange") +
  labs(title = "Capitec & SA Government Bonds Correlation", x = "Capitec Returns", y = "SA Bond Returns") + 
  theme_classic()

### CPI vs Market Indices

m_cum_ret <- cumprod(1 + M_Ind_ret) -1 
m_indexed_performance <- (m_cum_ret + 1) * 100

cm <- c("CPI" = "grey", "Top 40" = "green", "S&P 500" = "red", "Nasdaq 100" = "black", 
       "Euro STOXX 50" = "royalblue", "STOXX Europe 600" = "navy", "ASX 200" = "gold")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = Top40, col = "Top 40"), linewidth = 0.75) + 
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = SNP500, col = "S&P 500"), linewidth = 0.75) + 
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = NDX100, col = "Nasdaq 100"), linewidth = 0.75) + 
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = STOXX50, col = "Euro STOXX 50"), linewidth = 0.75) + 
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = STOXX600, col = "STOXX Europe 600"), linewidth = 0.75) +
  geom_line(m_indexed_performance, mapping = aes(x = index(m_indexed_performance), y = ASX200, col = "ASX 200"), linewidth = 0.75) +
  scale_color_manual(values = cm, guide = guide_legend(override.aes = list(fill = cm))) +
  labs(title = "CPI against Market Indices", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 

### CPI vs SA Real Estate

re_cum_ret <- cumprod(1 + RE_Index) -1 
re_indexed_performance <- (re_cum_ret + 1) * 100

cre <- c("CPI" = "grey", "Real Estate" = "brown")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(as.data.frame(re_indexed_performance), mapping = aes(x = index(re_indexed_performance), y  = Index, col = "Real Estate"), linewidth = 0.75) +
  scale_color_manual(values = cre, guide = guide_legend(override.aes = list(fill = cre))) +
  labs(title = "Capitec vs Real Estate Industry", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 

RE_plot <- merge(Banks_ret$CPI, RE_Index)

ggplot(as.data.frame(RE_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "brown", size = 2) + 
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Capitec & Real Estate Correlation", x = "Capitec Returns", y = "Real Estate Returns") + 
  theme_classic()

ggplot(as.data.frame(RE_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "brown", size = 2) + 
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  geom_smooth(method = lm, se = FALSE, col = "navy") +
  labs(title = "Capitec & Real Estate Correlation (Excluding outliers)", x = "Capitec Returns", y = "Real Estate Returns") + 
  theme_classic()

### CPI vs Retail Industry

retail_cum_ret <- cumprod(1 + SA_Retail_Index) -1 
retail_indexed_performance <- (retail_cum_ret + 1) * 100

cretail <- c("CPI" = "grey", "Retail Industry" = "red")

ggplot() + 
  geom_line(indexed_performance, mapping = aes(x = index(indexed_performance), y = CPI, col = "CPI"), linewidth = 0.75) + 
  geom_line(as.data.frame(retail_indexed_performance), mapping = aes(x = index(retail_indexed_performance), y  = Index, col = "Retail Industry"), linewidth = 0.75) +
  scale_color_manual(values = cretail, guide = guide_legend(override.aes = list(fill = cretail))) +
  labs(title = "Capitec vs Retail Industry", x = "Time", y = "Return", color = "Legend") +
  theme_classic() 

Retail_plot <- merge(Banks_ret$CPI, SA_Retail_Index)

ggplot(as.data.frame(Retail_plot), mapping = aes(x = CPI, y = Index)) + 
  geom_point(col = "red", size = 2) + 
  scale_x_continuous(limits = c(-0.1, 0.1)) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  geom_smooth(method=lm, se=FALSE, col = "navy") +
  labs(title = "Capitec & Retail Correlation", x = "Capitec Returns", y = "Retail Industry Returns") + 
  theme_classic()

#### BASIC STATS ####

Banking_Stats <- basicStats(cbind(SA_Bank_Index, Banks_ret))

Market_Stats <- basicStats(M_Ind_ret)

Other_Factors <- cbind(Treasury, GovBond_ret, RE_Index, SA_Retail_Index)
colnames(Other_Factors) <- c("US_rates", "SA_Bonds", "Real_Estate", "Retail")

OF_Stats <- basicStats(Other_Factors)

#### SIMPLE REGRESSIONS ####

CPIvBIndex <- lm(Banks_ret$CPI ~ SA_Bank_Index)
summary(CPIvBIndex)

CPIvBIndex <- lm(Banks_ret$CPI ~ SA_Bank_Index)
summary(CPIvBIndex) # We basically see CPI's beta against the Banking Industry
# Its a bit more volatile which meets expectations due to the lower stdev of the index due to diversification

CPIvTreas <- lm(Banks_ret$CPI ~ Treasury) # you need to sort out the length difference
summary(CPIvTreas)

CPIvGovBond <- lm(Banks_ret$CPI ~ GovBond_ret)
summary(CPIvGovBond)

CPIvTop40_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$Top40))
CPIvTop40 <- lm(CPIvTop40_ds$CPI ~ CPIvTop40_ds$Top40)
summary(CPIvTop40)

CPIvSNP_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$SNP500))
CPIvSNP <- lm(CPIvSNP_ds$CPI ~ CPIvSNP_ds$SNP500)
summary(CPIvSNP)

CPIvFTSE_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$FTSE))
CPIvFTSE <- lm(CPIvFTSE_ds$CPI ~ CPIvFTSE_ds$FTSE)
summary(CPIvFTSE)

CPIvSTOXX_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$STOXX600))
CPIvSTOXX <- lm(CPIvSTOXX_ds$CPI ~ CPIvSTOXX_ds$STOXX600)
summary(CPIvSTOXX)

CPIvASX_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$ASX200))
CPIvASX <- lm(CPIvASX_ds$CPI ~ CPIvASX_ds$ASX200)
summary(CPIvASX)

CPIvRE <- lm(Banks_ret$CPI ~ RE_Index)
summary(CPIvRE)

CPIvRetail <- lm(Banks_ret$CPI ~ SA_Retail_Index)
summary(CPIvRetail)

#### Unit-Root Tests ####

adf.test(SA_Bank_Index)
pp.test(SA_Bank_Index)

adf.test(GovBond_ret)
pp.test(GovBond_ret)

adf.test(M_Ind_ret$Top40)
pp.test(M_Ind_ret$Top40)

adf.test(M_Ind_ret$SNP500)
pp.test(M_Ind_ret$SNP500)

adf.test(M_Ind_ret$FTSE)
pp.test(M_Ind_ret$FTSE)

adf.test(M_Ind_ret$STOXX600)
pp.test(M_Ind_ret$STOXX600)

adf.test(M_Ind_ret$ASX200)
pp.test(M_Ind_ret$ASX200)

adf.test(RE_Index)
pp.test(RE_Index)

adf.test(SA_Retail_Index)
pp.test(SA_Retail_Index)

#### Optimal lag selection ####

SA_Banks_lag <- VARselect(na.omit(cbind(Banks_ret$CPI, SA_Bank_Index)), 
                            lag.max = 10, type = "const")
SA_Banks_lag$selection

Treasury_lag <- VARselect(na.omit(cbind(Banks_ret$CPI, Treasury)), 
                          lag.max = 10, type = "const")
Treasury_lag$selection

GovBond_lag <- VARselect(na.omit(cbind(Banks_ret$CPI, GovBond_ret)), 
                          lag.max = 10, type = "const")
GovBond_lag$selection

CPIvTop40_lag <- VARselect(CPIvTop40_ds, lag.max = 10, type = "const")
CPIvTop40_lag$selection

CPIvSNP_lag <- VARselect(CPIvSNP_ds, lag.max = 10, type = "const")
CPIvSNP_lag$selection

CPIvFTSE_lag <- VARselect(CPIvFTSE_ds, lag.max = 10, type = "const")
CPIvFTSE_lag$selection

CPIvSTOXX_lag <- VARselect(CPIvSTOXX_ds, lag.max = 10, type = "const")
CPIvSTOXX_lag$selection

CPIvASX_lag <- VARselect(CPIvASX_ds, lag.max = 10, type = "const")
CPIvASX_lag$selection

RE_Index_lag <- VARselect(na.omit(cbind(Banks_ret$CPI, RE_Index)),
                        lag.max = 10, type = "const")
RE_Index_lag$selection

SA_Retail_Index_lag <- VARselect(na.omit(cbind(Banks_ret$CPI, SA_Retail_Index)),
                        lag.max = 10, type = "const")
SA_Retail_Index_lag$selection #Interestingly this provides a 2-day lag order for some indicators

#### Banking Industry Analysis ####

# Johansen Co-Integration Test

SA_Banks_ds <- na.omit(cbind(Banks_ret$CPI, SA_Bank_Index))

SA_Banks_lag <- VARselect(SA_Banks_ds, lag.max = 10, type = "const")
SA_Banks_lag$selection

SA_Banks.jt <- ca.jo(SA_Banks_ds, type = "trace", ecdet = "none", K = 2)
summary(SA_Banks.jt) # test stat > 1% conf. lvl thus ts is cointegrated


# Vector Error Correction Model
 
SA_Banks_vecm <- VECM(SA_Banks_ds, lag = 1, r = 1, estim = "ML")
summary(SA_Banks_vecm)

SA_Banks_vecm$coefficients

# interesting results. CPI and Index can predict CPI results with a significant ECT
# Therefore there is autocorrelation in the CPI

# Impulse Response Functions

SB_vec2var <- vec2var(SA_Banks.jt)

SA_Banks_irf <- irf(SB_vec2var)
plot(SA_Banks_irf)

# Granger Causality

SA_Banks_gc <-  causality(VAR(SA_Banks_ds, p =1, type = "const"), cause = "Index")
SA_Banks_gc

#### Treasury Analysis ####

# Johansen Co-Integration Test

Treasury_ds <- na.omit(cbind(Banks_ret$CPI, Treasury))

Treasury_lag <- VARselect(Treasury_ds, lag.max = 10, type = "const")
Treasury_lag$selection

Treasury.jt <- ca.jo(Treasury_ds, type = "trace", ecdet = "none", K = 2)
summary(Treasury.jt) # test failed, although it was close

# VAR

Treasury_VAR <- VAR(Treasury_ds, p = 1, type = "const")
summary(Treasury_VAR) # Treasury would not be a great predictor for CPI returns

# Causlity

Treasury_gc <- causality(Treasury_VAR, cause = "FVX.Adjusted")
Treasury_gc # There's not a strong enough relationship

#### Government Bond Analysis ####

# Johansen Co-Integration 

GovBond_ds <- na.omit(cbind(Banks_ret$CPI, GovBond_ret))

GovBond_lag <- VARselect(GovBond_ds, lag.max = 10, type = "const")
GovBond_lag$selection

GovBond.jt <- ca.jo(GovBond_ds, type = "trace", ecdet = "none", K = 2)
summary(GovBond.jt) # test passed with flying colours

# VECM

GovBond_VECM <- VECM(GovBond_ds, lag = 1, r = 1, estim = "ML")
summary(GovBond_VECM)

summary(VAR(GovBond_ds, p = 1, type = "const"))

# causality 

GovBond_gc <-  causality(VAR(GovBond_ds, p =1, type = "const"), cause = "Index")
GovBond_gc

# Correlation test passed but there was no significant relationship with CPI

#### Top 40 Analysis ####

#Johansen Co-Integration

CPIvTop40_lag <- VARselect(CPIvTop40_ds, lag.max = 10, type = "const")
CPIvTop40_lag$selection

CPIvTop40.jt <- ca.jo(CPIvTop40_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvTop40.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvTop40_VECM <- VECM(CPIvTop40_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvTop40_ds)

# Causlity

CPIvTop40_gc <- causality(VAR(CPIvTop40_ds, p =1, type = "const"), cause = "Top40")
CPIvTop40_gc

#### S&P 500 Analysis ####

CPIvSNP_lag <- VARselect(CPIvSNP_ds, lag.max = 10, type = "const")
CPIvSNP_lag$selection

CPIvSNP.jt <- ca.jo(CPIvSNP_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvSNP.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvSNP_VECM <- VECM(CPIvSNP_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvSNP_VECM) # No Significant ECT but the past performances predict future performance of CPI

# VAR

CPIvSNP_VAR <- VAR(CPIvSNP_ds, p =1, type = "const")
summary(CPIvSNP_VAR)

# causality

CPIvSNP_gc <- causality(CPIvSNP_VAR, cause = "SNP500")
CPIvSNP_gc

#### Nasdaq Analysis ####

CPIvNDX_ds <- na.omit(cbind(Banks_ret$CPI, M_Ind_ret$NDX100))


CPIvNDX_lag <- VARselect(CPIvNDX_ds, lag.max = 10, type = "const")
CPIvNDX_lag$selection

CPIvNDX.jt <- ca.jo(CPIvNDX_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvNDX.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvNDX_VECM <- VECM(CPIvNDX_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvNDX_VECM) # NDX > SNP

# VAR

CPIvNDX_VAR <- VAR(CPIvNDX_ds, p =1, type = "const")
summary(CPIvNDX_VAR)

# causality

CPIvNDX_gc <- causality(CPIvNDX_VAR, cause = "NDX100")
CPIvNDX_gc

#### FTSE 100 Analysis ####

CPIvFTSE_lag <- VARselect(CPIvFTSE_ds, lag.max = 10, type = "const")
CPIvFTSE_lag$selection

CPIvFTSE.jt <- ca.jo(CPIvFTSE_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvFTSE.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvFTSE_VECM <- VECM(CPIvFTSE_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvFTSE_VECM) # Significant

# VAR

CPIvFTSE_VAR <- VAR(CPIvFTSE_ds, p =1, type = "const")
summary(CPIvFTSE_VAR)

# causality

CPIvFTSE_gc <- causality(CPIvFTSE_VAR, cause = "FTSE")
CPIvFTSE_gc

#### STOXX 600 Analysis ####

CPIvSTOXX_lag <- VARselect(CPIvSTOXX_ds, lag.max = 10, type = "const")
CPIvSTOXX_lag$selection

CPIvSTOXX.jt <- ca.jo(CPIvSTOXX_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvSTOXX.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvSTOXX_VECM <- VECM(CPIvSTOXX_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvSTOXX_VECM) # Significant

# VAR

CPIvSTOXX_VAR <- VAR(CPIvSTOXX_ds, p =1, type = "const")
summary(CPIvFTSE_VAR)

# causality

CPIvSTOXX_gc <- causality(CPIvSTOXX_VAR, cause = "STOXX600")
CPIvSTOXX_gc

#### ASX 200 Analysis ####

CPIvASX_lag <- VARselect(CPIvASX_ds, lag.max = 10, type = "const")
CPIvASX_lag$selection

CPIvASX.jt <- ca.jo(CPIvASX_ds, type = "trace", ecdet = "none", K = 2)
summary(CPIvASX.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# VECM

CPIvASX_VECM <- VECM(CPIvASX_ds, lag = 1, r = 1, estim = "ML")
summary(CPIvASX_VECM) # Significant

# VAR

CPIvASX_VAR <- VAR(CPIvASX_ds, p =1, type = "const")
summary(CPIvASX_VAR)

# causality

CPIvASX_gc <- causality(CPIvASX_VAR, cause = "ASX200")
CPIvASX_gc #There's causality!!!!

#### Real Estate Index ####

# Johansen Co-Integration Test

RE_ds <- na.omit(cbind(Banks_ret$CPI, RE_Index))

RE_lag <- VARselect(RE_ds, lag.max = 10, type = "const")
RE_lag$selection

RE.jt <- ca.jo(RE_ds, type = "trace", ecdet = "none", K = 2)
summary(RE.jt) # test stat > 1% conf. lvl thus ts is cointegrated


# Vector Error Correction Model

RE_vecm <- VECM(RE_ds, lag = 1, r = 1, estim = "ML")
summary(RE_vecm)

# VAR 

RE_VAR <- VAR(RE_ds, p = 1, type = "const")
summary(RE_VAR)

# Granger Causality

RE_gc <-  causality(RE_VAR, cause = "Index")
RE_gc

#### Retail Index ####

# Johansen Co-Integration Test

Retail_ds <- na.omit(cbind(Banks_ret$CPI, SA_Retail_Index))

Retail_lag <- VARselect(Retail_ds, lag.max = 10, type = "const")
Retail_lag$selection

Retail.jt <- ca.jo(Retail_ds, type = "trace", ecdet = "none", K = 2)
summary(Retail.jt) # test stat > 1% conf. lvl thus ts is cointegrated

# Vector Error Correction Model

Retail_vecm <- VECM(Retail_ds, lag = 1, r = 1, estim = "ML")
summary(Retail_vecm)

# VAR 

Retail_VAR <- VAR(Retail_ds, p = 1, type = "const")
summary(Retail_VAR)

# Granger Causality

Retail_gc <-  causality(Retail_VAR, cause = "Index")
Retail_gc # causality!!!!

#### Correlation ####

factors_corr <- cor(
  na.omit(
    cbind(SA_Bank_Index, Treasury, GovBond_ret, M_Ind_ret, RE_Index, SA_Retail_Index)
  )
)

# To summarize the results the only severe correlation is between SNP & Nasdaq
# Thus all significant relationships explain something unique about CPI performance.
# Only SNP 500 will be omitted from this model

#### Simple Neural Net ####


CPI_factors <- cbind(SA_Bank_Index, Treasury, GovBond_ret, M_Ind_ret, RE_Index, SA_Retail_Index)

#Optimisation

# We can use the p-values from our models as the targets for our optimisation model
# to get optimal weights. Models with the lowest p-values will be assigned larger weights.

# ret_perf <- rportf[paste(init_date,latest_date, sep = "::")]

Day1 <- Sys.Date() - 740
DayL <- Sys.Date()
Day1_end <- Sys.Date() - 185
DayL_start <- Sys.Date() - 184

Full_ds <- na.omit(cbind(Banks_ret$CPI, CPI_factors))
Training_set <- Full_ds[paste(Day1, Day1_end, sep = "::")]
Test_set <- Full_ds[paste(DayL_start, DayL, sep = "::")]

test_nnet <- neuralnet(CPI ~ Bank_Index + FVX.Adjusted + GovBond_Index + Top40 + 
                         SNP500 + NDX100 + FTSE + STOXX600 + ASX200 + RE_Index + 
                         Retail_Index, 
                      data = Training_set, hidden = c(7, 3), rep = 10)

plot(test_nnet)

test_pred <- predict(test_nnet, Test_set)
test_pred <- as.xts(test_pred)
colnames(test_pred) <- c("CPIe")
index(test_pred) <- as.Date(index(test_pred))

ggplot() +
  geom_line(data = Test_set, mapping = aes(x = index(Test_set), y = CPI), col = "black", linewidth = 0.75) + 
  geom_line(data = test_pred, mapping = aes(x = index(test_pred), y = CPIe), col = "red", linewidth = 0.75) +
  theme_classic() 

# Testing Predictive Power

data_v_pred <- lm(Test_set$CPI ~ test_pred$CPIe)
model_r2 <- summary(data_v_pred)$r.squared


# The neural net has sum predicted a few return fluctuations greater than 5% in absolute terms which is pretty impressive.
# Although the model can be improved.

# Well done mate!!!

# Areas for improvement
# We can include the Nikkei index & Bitcoin to see their predictive power.
# Check coefficient of determination and mse to see the prediction against the test data's performance

# NNet test using lagged explanatory variables

lagged_x_var <- lag(Full_ds[, 2:12])
lagged_ds <- na.omit(
  cbind(Full_ds$CPI, lagged_x_var)
)

Training_set_2 <- lagged_ds[paste(Day1, Day1_end, sep = "::")]
Test_set_2 <- lagged_ds[paste(DayL_start, DayL, sep = "::")]

test_nnet_2 <- neuralnet(CPI ~ Bank_Index + FVX.Adjusted + GovBond_Index + Top40 + 
                         SNP500 + NDX100 + FTSE + STOXX600 + ASX200 + RE_Index + 
                         Retail_Index, 
                       data = Training_set_2, hidden = c(7, 3), rep = 10)

plot(test_nnet_2)

test_pred_2 <- predict(test_nnet_2, Test_set_2)
test_pred_2 <- as.xts(test_pred_2)
colnames(test_pred_2) <- c("CPIe")
index(test_pred_2) <- as.Date(index(test_pred_2))

ggplot() +
  geom_line(data = Test_set_2, mapping = aes(x = index(Test_set_2), y = CPI), col = "black", linewidth = 0.75) + 
  geom_line(data = test_pred_2, mapping = aes(x = index(test_pred_2), y = CPIe), col = "red", linewidth = 0.75) +
  theme_classic() 

data_v_pred_2 <- lm(Test_set_2$CPI ~ test_pred_2$CPIe)
model_r2_2 <- summary(data_v_pred_2)$r.squared
