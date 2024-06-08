library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(pso)
library(fPortfolio)

getSymbols(c("STX500.JO", "STX40.JO", "STXNDQ.JO", "STXEMG.JO", 
             "SYGWD.JO", "PPE.JO", "SDO.JO", "ARKK", "RIVN", "CPI.JO",
             "BTC-USD", "ETH-USD", "UNI7083-USD", "PAH3.DE","COH.JO"), 
           from = Sys.Date()-381,
           to = Sys.Date()-1)

getSymbols(c("ZAR=X", "EURZAR=X"), 
           from = Sys.Date()-381,
           to = Sys.Date()-1)

portf <- cbind(STX500.JO$STX500.JO.Adjusted, STX40.JO$STX40.JO.Adjusted, 
               STXNDQ.JO$STXNDQ.JO.Adjusted, STXEMG.JO$STXEMG.JO.Adjusted, 
               SYGWD.JO$SYGWD.JO.Adjusted, PAH3.DE$PAH3.DE.Adjusted, 
               CPI.JO$CPI.JO.Adjusted, PPE.JO$PPE.JO.Adjusted, 
               SDO.JO$SDO.JO.Adjusted, COH.JO$COH.JO.Adjusted, ARKK$ARKK.Adjusted,
               RIVN$RIVN.Adjusted, `BTC-USD`$`BTC-USD.Adjusted`, 
               `ETH-USD`$`ETH-USD.Adjusted`, `UNI7083-USD`$`UNI7083-USD.Adjusted`)

colnames(portf) <- c("STX500", "STX40", "STX100", "STXEMG", "SYGWD", "PAH3", 
                     "CPI", "PPE", "SDO", "COH", "ARKK", "RIVN", "BTC", "ETH", "UNI")

portf <- na.omit(portf)

rportf <- Return.calculate(portf, method = "discrete")

rportf <- na.omit(rportf)

#Price and return plots

#Correlations
#add other statistics like basic descriptions etc.
#portfolio return distributions 

corr <- cor(rportf)
view(corr)

#portfolio mean and variances

p.mean <- mean(rportf)
p.stdev <- sd(rportf)

asset_mean <- as.data.frame(lapply(rportf, mean))
annualised_mean <- asset_mean*252 #Calc. Geom_means instead of arithmetic means

ag.mean <- mean.geometric(rportf)
aag.mean <- ag.mean*252
ave.p.ret. <- mean(aag.mean) #Ave. portfolio return as of 15/01/24 is 22.15%. Therefore, with optimisation we can push for 30%-40%
#Thus target return is 35% p.a.

asset_sd <- as.data.frame(lapply(rportf, sd))
annualised_sd <- asset_sd*sqrt(252)
ave.p.sd <- mean(t(annualised_sd))

#Constraints

rportf_ts <- as.timeSeries(rportf)

p_spec <- portfolioSpec()
setTargetReturn(p_spec) <- mean(ag.mean)*1.58 #we want our portfolio to perform 58% better than the average equally weighted return

defaultConstraints <- portfolioConstraints(rportf_ts, spec = p_spec, 
                                           constraints = c("LongOnly", "minW = 0", "maxW = 1"))

tgtportfolio <- efficientPortfolio(rportf_ts, spec = p_spec, 
                                   constraints = c("LongOnly", "minW = 0", "maxW = 1"))

summary(tgtportfolio)

#get the weights

tgtweights <- getWeights(tgtportfolio) %>%
  t() %>%
  as.data.frame()

#

crnt_price <- tail(portf, n = 1)

holdings <- t(c(1.7310, 0.1438, 0.062, 1.1164, 1.0012, 0, 0, 10,
                1.9793, 0, 0.9962, 0.00001, 0, 0.44866))
colnames(holdings) <- c("STX500", "STX40", "STX100", "STXEMG", "MSCI", "PAH3",  
                        "CPI", "PPE", "SDO", "COH", "ARKK", "RIVN", "BTC", "ETH", "UNI")

unadj_stock_values <- crnt_price*holdings

EUR.ZAR <- `EURZAR=X`$`EURZAR=X.Adjusted`*100
USD.ZAR <- `ZAR=X`$`ZAR=X.Adjusted`*100

stock_values <- unadj_stock_values
stock_values$PAH3 <- unadj_stock_values$PAH3*EUR.ZAR
stock_values$ARKK <- unadj_stock_values$ARKK*USD.ZAR
stock_values$RIVN <- unadj_stock_values$RIVN*USD.ZAR
stock_values$BTC <- unadj_stock_values$BTC*USD.ZAR
stock_values$ETH <- unadj_stock_values$ETH*USD.ZAR
stock_values$UNI <- unadj_stock_values$UNI*USD.ZAR

stock_values <- stock_values/100

Ttl_value <- sum(stock_values)
act_wts <- stock_values/Ttl_value

buy.sell_dec <- as.data.frame(tgtweights)-as.data.frame(act_wts) # if values are positive - buy, if negative - sell.

#current stock price converted from local currency to ZAR calculation

crnt_stck_prices <- crnt_price

crnt_stck_prices$PAH3 <- crnt_price$PAH3*EUR.ZAR
crnt_stck_prices$ARKK <- crnt_price$ARKK*USD.ZAR
crnt_stck_prices$RIVN <- crnt_price$RIVN*USD.ZAR
crnt_stck_prices$BTC <- crnt_price$BTC*USD.ZAR
crnt_stck_prices$ETH <- crnt_price$ETH*USD.ZAR
crnt_stck_prices$UNI <- crnt_price$UNI*USD.ZAR

crnt_stck_prices <- crnt_stck_prices/100

#creating buy/sell signals

buy.sell_units <- round((buy.sell_dec*Ttl_value)/as.data.frame(crnt_stck_prices), 2)
indicator <- ifelse(as.data.frame(buy.sell_units) > 0.00, "BUY", 
                    ifelse(as.data.frame(buy.sell_units) < 0.00, "SELL", "HOLD"))

#combining buy/sell decision with no. of units to buy/sell

row.names(indicator) <- 1
row.names(buy.sell_units) <- 2

p.overview <- rbind(as.data.frame(indicator), as.data.frame(buy.sell_units))
view(p.overview)

#pulling fundamental asset valuations from excel file

Asset_Valuations <- read_excel("Asset Valuations.xlsx")

#Converting SA stocks from cents to rands

crnt_price2 <- crnt_price
crnt_price2$CPI <- crnt_price$CPI/100
crnt_price2$PPE <- crnt_price$PPE/100
crnt_price2$SDO <- crnt_price$SDO/100
crnt_price2$COH <- crnt_price$COH/100

est.value <- Asset_Valuations[1, -1] 
buy.price <- Asset_Valuations[2, -1]

final_valuation <- ifelse(crnt_price2>est.value, "Overvalued", 
                          ifelse(crnt_price2<est.value & 
                                   crnt_price2>buy.price, "Hezzy", "Undervalued")) #"Hezzy" is short for "Hesitant" lol

inv_table <- rbind(p.overview, final_valuation)

view(inv_table)
