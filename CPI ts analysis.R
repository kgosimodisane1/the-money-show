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
