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

bank_market_values = pd.read_excel(io="C:/Users/lenovo/OneDrive/Documents/SA Banks Market Cap.xlsx", sheet_name="Sheet1")

print(CPI.info()) # calc. adj. closing price using close, dividends & stock splits
