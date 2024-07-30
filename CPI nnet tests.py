import pandas
import pandas_datareader as pdr

# REQUESTING DATA FROM YAHOO FINANCE API

# Banking Industry

CPI = pdr.get_data_yahoo('CPI.JO') # Capitec
ABG = pdr.get_data_yahoo('ABG.JO') # ABSA
SBK = pdr.get_data_yahoo('SBK.JO') # Standard Bank
FSR = pdr.get_data_yahoo('FSR.JO') # First Rand
NED = pdr.get_data_yahoo('NED.JO') # Nedbank
INL = pdr.get_data_yahoo('INL.JO') # Investec
