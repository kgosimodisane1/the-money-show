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

Banking_Index = Banking_Index.dropna()
