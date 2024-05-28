rm(list=ls())

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Specification of machine: AMD Ryzen Threadripper PRO 5965WX 24_Cores, 3801 MHz, 24 Core(s), 48 Logical Processor(s)

# Libraries
library(rstudioapi) # version 0.16.0 
library(forecast) # version 8.22.0 
library(randomForest) # version 4.7-1.1
library(tidyverse) # version 2.0.0
library(janitor) # version 2.2.0
library(caret) # version 6.0-94
library(xgboost) # version 1.7.7.1
library(FoReco) # version 0.2.6
library(fastDummies) # version 1.7.3
library(lightgbm) # version 4.3.0
library(rBayesianOptimization) # version 1.2.0
library(mlr) # version 2.19.1

# Main functions
setwd("../")
source('functions.R')
source('forecast_evaluation_functions.R')

# Load Data
setwd("./Data")
load("citibike.RData")

setwd(".././sensitivity")
##################################################################
########################### Naive Base ###########################
##################################################################
base_method = "naive"

# Sensitivity on features
ML_method = "randomforest"; source('ML-foreco-sensitivity-features.R')  # This script runs the ML forecast reconciliation using the complete feature: and took 142min
ML_method = "xgboost"; source('ML-foreco-sensitivity-features.R') # This script took 20min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-features.R') # This script took 30min

# Sensitivity on frequencies
ML_method = "randomforest"; source('ML-foreco-sensitivity-frequencies.R')  # This script runs the ML forecast reconciliation using only the three temporal frequencies of interest and took 28min
ML_method = "xgboost"; source('ML-foreco-sensitivity-frequencies.R') # This script took 16min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-frequencies.R') # This script took 28min


################################################################
########################### ETS Base ###########################
################################################################
base_method = "ets"

# Sensitivity on features
ML_method = "randomforest"; source('ML-foreco-sensitivity-features.R')  # This script runs: and took 133min
ML_method = "xgboost"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 20min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 31min

# Sensitivity on frequencies
ML_method = "randomforest"; source('ML-foreco-sensitivity-frequencies.R')  # This script runs: and took 25min
ML_method = "xgboost"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 16min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 28min
ML_method = "bottomup"; source('ML-foreco-sensitivity-frequencies.R')  

FoReco_method = "tcs"; source('linear-foreco-sensitivity-frequencies.R') # This script runs the linear forecast reconciliation using only the three temporal frequencies of interest
FoReco_method = "cst"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "oct"; source('linear-foreco-sensitivity-frequencies.R')


###################################################################
########################### Sarima Base ###########################
###################################################################
base_method = "sarima"

# Sensitivity on features
ML_method = "randomforest"; source('ML-foreco-sensitivity-features.R')  # This script runs: and took 137min
ML_method = "xgboost"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 20min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 31min

# Sensitivity on frequencies
ML_method = "randomforest"; source('ML-foreco-sensitivity-frequencies.R')  # This script runs: and took 26min
ML_method = "xgboost"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 16min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 28min
ML_method = "bottomup"; source('ML-foreco-sensitivity-frequencies.R')  

FoReco_method = "tcs"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "cst"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "oct"; source('linear-foreco-sensitivity-frequencies.R')


#################################################################################
########################### Forecast Combination Base ###########################
#################################################################################
base_method = "averageBF"

# Sensitivity on features
ML_method = "randomforest"; source('ML-foreco-sensitivity-features.R')  # This script runs: and took 140min
ML_method = "xgboost"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 20min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-features.R') # This script runs: and took 31min

# Sensitivity on frequencies
ML_method = "randomforest"; source('ML-foreco-sensitivity-frequencies.R')  # This script runs: and took 26min
ML_method = "xgboost"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 16min
ML_method = "lightgbm"; source('ML-foreco-sensitivity-frequencies.R') # This script runs: and took 28min
ML_method = "bottomup"; source('ML-foreco-sensitivity-frequencies.R') 

FoReco_method = "tcs"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "cst"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco-sensitivity-frequencies.R')
FoReco_method = "oct"; source('linear-foreco-sensitivity-frequencies.R')

###############################################################
########################### Results ###########################
###############################################################
# WAPE
source('Tables_B1.R'); round(TableB1_H3cells, 4); round(TableB1_market, 4)
source('Tables_B2.R'); round(TableB2, 4)
save(TableB1_H3cells, file = "TableB1_H3cells.RData")
save(TableB1_market, file = "TableB1_market.RData")
save(TableB2, file = "TableB2.RData")

