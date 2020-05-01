# -*- coding: utf-8 -*-
"""
Created on Thu Apr  2 12:56:27 2020

@author: Jayesh
"""

#%%
import pandas as pd 
import numpy as np 
import os 
#%%
""" setting up directory and data files""" 

os.chdir("C:\\Users\\Jayesh\\Downloads\\DIIG\\S20\\MW") #setting directory 

"""
note that in these data columns all of the probabilities were generated using Baye's theorem and all of the probability columns reflect 
these values other than the device csv file where the Baye's column reflects the device's baye probability
"""

gender = pd.read_csv('final_final_product_reviews_final_gender.csv') #getting information on gender analysis 
condition = pd.read_csv("final_final_product_reviews_final_condition.csv") #getting information on medical condition analysis 
case = pd.read_csv("final_final_product_reviews_final_case.csv") #getting information on case use analysis 
device = pd.read_csv("final_final_product_reviews_products_devices_count.csv") #getting information on previous device analysis 

#%%
"""This is for the user to input all their information about themselves and their wants"""
gender_input =  input("Please either either male or female: ")  #stores gender 
condition_input = input("Please input what condition you have: ") #stores medical condition  
case_input = input("Please input what function you desire from the device: ")  #stores user need   
device_input = input("Please input what device you want to replace: ") #stores user previous device   
#%%
""" this is all the default scores for each of the device"""
score_medplanner = 0
score_mighty_pages = 0
score_merch = 0
score_picc_line_cover = 0
score_the_mighty_wrap = 0
score_medical_iv_fusion_backpack = 0
#%%
""" this is all the weightings for each condition"""
condition['probability'] = condition['probability'] * 0.4
case['probability'] = case['probability'] * 0.3
device['probability'] = device['probability'] * 0.2
gender['probability'] = gender['probability'] * 0.1 
#%%
""" this is the summing function, you input which device is being examined and the score it recieves for each condition above"""
def summing(stringinput, scoreinput, ):
    global score_medplanner, score_mighty_pages, score_merch, score_picc_line_cover, score_the_mighty_wrap, score_medical_iv_fusion_backpack
    if (stringinput == "medical-iv-infusion-backpack-for-patients"):
        score_medical_iv_fusion_backpack += scoreinput 
    if (stringinput == "medplanner"):
        score_medplanner += scoreinput 
    if (stringinput == "merch"):
        score_merch += scoreinput 
    if (stringinput == "mighty-pages"):
        score_mighty_pages += scoreinput 
    if (stringinput == "picc line cover"):
        score_picc_line_cover += scoreinput 
    if (stringinput == "the-mighty-wrap"):
        score_the_mighty_wrap += scoreinput 
#%%
"""this is finding out which device has the highest score"""
def max_score():
    global score_medplanner, score_mighty_pages, score_merch, score_picc_line_cover, score_the_mighty_wrap, score_medical_iv_fusion_backpack
    max_val = max(score_medplanner, score_mighty_pages, score_merch, score_picc_line_cover, score_the_mighty_wrap, score_medical_iv_fusion_backpack)
    if (max_val == score_medplanner):
        print ("The best device to use is the medplanner") 
    if (max_val == score_mighty_pages):
        print ("The best device to use is the mighty pages") 
    if (max_val == score_merch):
        print ("The best device to use is the merch")  
    if (max_val == score_picc_line_cover):
        print ("The best device to use is the picc line cover") 
    if (max_val == score_the_mighty_wrap):
        print ("The best device to use is the mighty wrap") 
    if (max_val == score_medical_iv_fusion_backpack):
        print ("The best device to use is the medical iv infusion backpack") 
        
#%%
""" this is about inputting the user information found above for each category and calculating the score for each device"""
product_devices = ["medical-iv-infusion-backpack-for-patients", "medplanner", "merch", "mighty-pages", "picc line cover", "the-mighty-wrap"]
score = 0.0 #temp value that changes 
for string in product_devices:
    if condition_input in condition['condition'].unique() : #first checking if user input is in the database for condition 
        index_of_condition = condition.index[(condition['condition'] == condition_input) & (condition['product_handle'] == string)].tolist()[0] #getting the index of the condiiton and device to find its probability 
        score = condition['probability'][index_of_condition] #getting probability of the two conditions above 
        summing(string, score) #adding the probability above to the scores of each device  
    else:
        score = 0.0 
    if case_input in case['use.case'].unique(): #repeating above for use case 
        index_of_condition = case.index[(case['use.case'] == case_input) & (case['product_handle'] == string)].tolist()[0]
        score = case['probability'][index_of_condition]
        summing(string, score)
    else:
        score = 0.0 
    if gender_input in gender['gender'].unique(): #repeating above for gender 
        index_of_condition = gender.index[(gender['gender'] == gender_input) & (gender['product_handle'] == string)].tolist()[0]
        score = gender['probability'][index_of_condition]
        summing(string, score) 
    else:
        score = 0.0
    if device_input in device['device'].unique(): #repeating above for previous device 
        index_of_condition = device.index[(device['device'] == device_input) & (device['product_handle'] == string)].tolist()[0]
        score = device['probability'][index_of_condition]
        summing(string, score) 
    else:
        score = 0.0
#%%
""" printing the max score"""
max_score()
    
    