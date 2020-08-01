# -*- coding: utf-8 -*-
"""
Created on Tue Aug 13 13:48:32 2019
the upper triangle of the matrix is stored by row (condensed matrix). If m is the number of variables, 
then for i < j < m, the statistic between (row) i and j is stored in k = m*i - i*(i+1)/2 - i - 1 + j. 
The length of the vectors is n = m*(m-1)/2
@author: SHU
"""
import glob, os
#import numpy as np
import pandas as pd
#import matplotlib.pyplot as plt
from minepy import pstats
import re

#os.chdir("I:/DV_nonconf/20170428CGB-BT10m/MIC/") #directory

files = glob.glob("*.csv")

for file in files:
    name = re.search('(.+?).csv',file).group(1)  # substring by pattern
    df= pd.read_csv(file)
    df1 = df.drop(df.columns[[0]], axis=1) # delete the 1st col index
    #df1.to_csv('dat-saved.csv') #save df to csv
    a = df1.to_numpy()  # w/o col names; same as 'a = np.asarray(df1)'
    b = a.T
    mics, tics = pstats(b, alpha=9, c=5, est="mic_e")
    mics = pd.DataFrame(mics)
    mics.to_csv('Mic_' + name + '.csv')

