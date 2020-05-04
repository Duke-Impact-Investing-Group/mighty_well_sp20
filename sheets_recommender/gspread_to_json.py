# -*- coding: utf-8 -*-
"""
Created on Mon May  4 09:40:44 2020

@author: Aneesh
"""
import json
import gspread
import pandas as pd
from oauth2client.service_account import ServiceAccountCredentials


# Google spreadsheets bot config - secret file
with open('./config/googleconfig.json') as data_file:
        CONFIG = json.load(data_file)

credential_json = CONFIG['credentials']

scope = ['https://spreadsheets.google.com/feeds']
credentials = ServiceAccountCredentials.from_json_keyfile_dict(
    credential_json, scope)
client = gspread.authorize(credentials)

# Open the google sheets using its key - it's a part of the URL
# I stored the key in a config.json file with the other credentials
sheet = client.open_by_key(CONFIG['mightywell-key']).sheet1


# convert google sheet to DF
df = pd.DataFrame(sheet.get_all_records(head=2))
cols = list(df.columns)
# reorder columns
df = df[[cols[0], cols[1], cols[3], cols[2]]]

nan_value = float("NaN")
df.replace("", nan_value, inplace=True)
df = df.dropna(subset = [cols[3]], inplace=False)
df.reset_index(inplace = True)
df.drop(columns = ["index"])

# missing rows are populated with device name from row above
for i, row in df.iterrows():
    unique_id = i
    device_name = row[cols[1]]
    if(str(device_name) == "nan"):
        df.loc[i, cols[1]] = df.loc[i-1, cols[1]]

# now we make a device -> product recommended map
device_to_product = dict()

for i, row in df.iterrows():
    unique_id = i
    device_name = row[cols[1]]
    product_name = row[cols[3]]
    if not device_name in device_to_product:
        device_to_product[device_name] = []
    
    device_to_product[device_name].append(product_name)

# write dictionary out to jSON
with open('./data/device_prod.json', 'w') as json_file:
  json.dump(device_to_product, json_file)

	

