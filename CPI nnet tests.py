# import pandas_datareader as pdr
import pandas as pd
# import openpyxl
import yfinance as yf

# Request data via Yahoo public API
# CPI = pdr.get_data_yahoo('CPI.JO')

CPI = yf.Ticker("CPI.JO").history(period = '5y')
ABG = yf.Ticker("ABG.JO").history(period = '5y')
SBK = yf.Ticker("SBK.JO").history(period = '5y')
NED = yf.Ticker("NED.JO").history(period = '5y')
FSR = yf.Ticker("FSR.JO").history(period = '5y')
INL = yf.Ticker("INL.JO").history(period = '5y')

# Pulling Market Cap Data

bank_market_values = pd.read_excel(io="C:/Users/lenovo/OneDrive/Documents/SA Banks Market Cap.xlsx", sheet_name="Banking")
bank_market_values.columns = ['CPI', 'ABG', 'SBK', 'FSR', 'NED', 'INL']

# Calculating weights

banking_mcap = bank_market_values.iloc[0].sum()
bank_wts = bank_market_values/banking_mcap

# Calculating Returns

CPI['Return'] = CPI['Close'].pct_change()
ABG['Return'] = ABG['Close'].pct_change()
SBK['Return'] = SBK['Close'].pct_change()
FSR['Return'] = FSR['Close'].pct_change()
NED['Return'] = NED['Close'].pct_change()
INL['Return'] = INL['Close'].pct_change()

# Adding returns and weights to Banking Index

Banking_Index = pd.DataFrame()

Banking_Index['Return'] = (CPI['Return']*bank_wts['CPI'].iloc[0] + ABG['Return']*bank_wts['ABG'].iloc[0] +
                           SBK['Return']*bank_wts['SBK'].iloc[0] + FSR['Return']*bank_wts['FSR'].iloc[0] +
                           NED['Return']*bank_wts['NED'].iloc[0] + INL['Return']*bank_wts['INL'].iloc[0])

# Banking_Index = Banking_Index.dropna()

# US Treasury

Treasury = yf.Ticker("^FVX").history(period='5y')['Close']
Treasury.name = 'Treasury'

# Top 40

Top40 = yf.Ticker("^J200.JO").history(period='5y')['Close']
Top40_r = Top40.pct_change()
Top40_r.name = 'Top40'

# Nasdaq

NDX = yf.Ticker("^NDX").history(period='5y')['Close']
NDX_r = NDX.pct_change()
NDX_r.name = 'NDX'

# FTSE 100

FTSE = yf.Ticker("^FTSE").history(period='5y')['Close']
FTSE_r = FTSE.pct_change()
FTSE_r.name = 'FTSE'

# Stoxx 600

STOXX = yf.Ticker("^STOXX").history(period='5y')['Close']
STOXX_r = STOXX.pct_change()
STOXX_r.name = 'STOXX'

# ASX 200

ASX = yf.Ticker("^AXJO").history(period='5y')['Close']
ASX_r = ASX.pct_change()
ASX_r.name = 'ASX'

# SA Real Estate

RE_tickers = ['GRT.JO', 'RDF.JO', 'FFB.JO', 'RES.JO', 'VKE.JO', 'HYP.JO', 'ATT.JO', 'SSS.JO', 'SAC.JO', 'EMI.JO', 'ACS.JO', 'OCT.JO', 'SAR.JO', 'BWN.JO', 'APF.JO', 'TEX.JO', 'TMT.JO', 'PPR.JO', 'DIB.JO']

RE_prices = pd.DataFrame()

for RE_ticker in RE_tickers:
    RE_stocks = yf.Ticker(RE_ticker).history(period='5y')
    #   hist = stock.history(period='5y') supposed to be like this in case the above line fails

    RE_prices[RE_ticker] = RE_stocks['Close']
#    RE_prices.columns = ['GRT', 'RDF', 'FFB', 'RES', 'VKE', 'HYP', 'ATT', 'SSS', 'SAC', 'EMI', 'ACS', 'OCT', 'SAR', 'BWN', 'APF', 'TEX', 'TMT', 'PPR', 'DIB']


RE_returns = RE_prices.pct_change()

# RE stock returns

RE_values = pd.read_excel(io="C:/Users/lenovo/OneDrive/Documents/SA Banks Market Cap.xlsx", sheet_name="Real Estate")
RE_values.columns = ['GRT', 'RDF', 'FFB', 'RES', 'VKE', 'HYP', 'ATT', 'SSS', 'SAC', 'EMI', 'ACS', 'OCT', 'SAR', 'BWN', 'APF', 'TEX', 'TMT', 'PPR', 'DIB']

# Calculating weights

RE_mcap = RE_values.iloc[0].sum()
RE_wts = RE_values/RE_mcap

RE_Index = pd.DataFrame()

RE_Index['Real_Estate'] = (RE_returns['GRT.JO']*RE_wts['GRT'].iloc[0] + RE_returns['RDF.JO']*RE_wts['RDF'].iloc[0] +
                      RE_returns['FFB.JO']*RE_wts['FFB'].iloc[0] + RE_returns['RES.JO']*RE_wts['RES'].iloc[0] +
                      RE_returns['VKE.JO']*RE_wts['VKE'].iloc[0] + RE_returns['HYP.JO']*RE_wts['HYP'].iloc[0] +
                      RE_returns['ATT.JO']*RE_wts['ATT'].iloc[0] + RE_returns['SSS.JO']*RE_wts['SSS'].iloc[0] +
                      RE_returns['SAC.JO']*RE_wts['SAC'].iloc[0] + RE_returns['EMI.JO']*RE_wts['EMI'].iloc[0] +
                      RE_returns['ACS.JO']*RE_wts['ACS'].iloc[0] + RE_returns['OCT.JO']*RE_wts['OCT'].iloc[0] +
                      RE_returns['SAR.JO']*RE_wts['SAR'].iloc[0] + RE_returns['BWN.JO']*RE_wts['BWN'].iloc[0] +
                      RE_returns['APF.JO']*RE_wts['APF'].iloc[0] + RE_returns['TEX.JO']*RE_wts['TEX'].iloc[0] +
                      RE_returns['TMT.JO']*RE_wts['TMT'].iloc[0] + RE_returns['PPR.JO']*RE_wts['PPR'].iloc[0] +
                      RE_returns['DIB.JO']*RE_wts['DIB'].iloc[0])

# Retail

retail_tickers = ['CLS.JO', 'DCP.JO', 'MRP.JO', 'TFG.JO', 'TRU.JO', 'WHL.JO', 'PIK.JO', 'SHP.JO', 'SPP.JO']

retail_prices = pd.DataFrame()

for retail_ticker in retail_tickers:
    retail_stocks = yf.Ticker(retail_ticker).history(period='5y')
    #   hist = stock.history(period='5y') supposed to be like this in case the above line fails

    retail_prices[retail_ticker] = retail_stocks['Close']
#    retail_prices.columns = ['CLS', 'DCP', 'MRP', 'TFG', 'TRU', 'WHL', 'PIK', 'SHP', 'SPP']

retail_returns = retail_prices.pct_change()

retail_values = pd.read_excel(io="C:/Users/lenovo/OneDrive/Documents/SA Banks Market Cap.xlsx", sheet_name="Retail")
retail_values.columns = ['CLS', 'DCP', 'MRP', 'TFG', 'TRU', 'WHL', 'PIK', 'SHP', 'SPP']

# Calculating weights

retail_mcap = retail_values.iloc[0].sum()
retail_wts = retail_values/retail_mcap

retail_Index = pd.DataFrame()

retail_Index['Retail'] = (retail_returns['CLS.JO']*retail_wts['CLS'].iloc[0] + retail_returns['DCP.JO']*retail_wts['DCP'].iloc[0] +
                          retail_returns['MRP.JO']*retail_wts['MRP'].iloc[0] + retail_returns['TFG.JO']*retail_wts['TFG'].iloc[0] +
                          retail_returns['TRU.JO']*retail_wts['TRU'].iloc[0] + retail_returns['WHL.JO']*retail_wts['WHL'].iloc[0] +
                          retail_returns['PIK.JO']*retail_wts['PIK'].iloc[0] + retail_returns['SHP.JO']*retail_wts['SHP'].iloc[0] +
                          retail_returns['SPP.JO']*retail_wts['SPP'].iloc[0])
