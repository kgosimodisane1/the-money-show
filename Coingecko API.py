import pandas as pd
import ast
import requests

# Pulling Bitcoin historical ohlc data @ 30-minute interval
url = "https://api.coingecko.com/api/v3/coins/bitcoin/ohlc?vs_currency=usd&days=1&precision=2&x_cg_demo_api_key=CG-bQSUCXhXMtB9rRiMfYZN1BkY"

headers = {"accept": "application/json"}

# Storing pull request as a text
response = requests.get(url, headers=headers)

print(response.text)

# Converting text to a data frame
data_list = ast.literal_eval(response.text)

df = pd.DataFrame(data_list, columns=['Timestamp', 'Open', 'High', 'Low', 'Close'])

# Converting date-time to a readable format
df['Timestamp'] = pd.to_datetime(df['Timestamp'], unit='ms')

print(df)
