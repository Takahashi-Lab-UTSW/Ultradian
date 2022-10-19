# -*- coding: utf-8 -*-
"""
Created on Tue Aug 13 13:48:32 2019
the upper triangle of the matrix is stored by row (condensed matrix). If m is the number of variables, 
then for i < j < m, the statistic between (row) i and j is stored in k = m*i - i*(i+1)/2 - i - 1 + j. 
The length of the vectors is n = m*(m-1)/2

login into BioHPC
create env ml37 ==========================
module load python  ## default python/3.7.x-anaconda, to work with conda
conda create -n ml37 python=3.7  ## create env ml37, installs python3.7.3
source activate ml37
conda install -c conda-forge pandas ## 0.25.1
# install mpi4py to env-ml(ml37)
module load openmpi/gcc/64/4.0.3
conda install mpi4py

allocate resources: srun -p super -N 1 --mem 252928 --pty --time=20-00:00:00 /bin/bash
ssh to allocated resource
conda activate ml37
python MIC.py


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
    name = re.search('(.+?)_norm.csv',file).group(1)  # substring by pattern
    df= pd.read_csv(file)
    df1 = df.drop(df.columns[[0]], axis=1) # delete the 1st col index
    a = df1.to_numpy()  # w/o col names; same as 'a = np.asarray(df1)'
    b = a.T
    mics, tics = pstats(b, alpha=9, c=5, est="mic_e")
    mics = pd.DataFrame(mics)
    mics.to_csv('Mic_' + name + '.csv')

