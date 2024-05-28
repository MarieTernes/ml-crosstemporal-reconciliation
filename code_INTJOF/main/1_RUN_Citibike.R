rm(list=ls())

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

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Main functions
setwd("../")
source('functions.R')
source('forecast_evaluation_functions.R')

# Load Data
setwd("./data")
load("citibike.RData")

setwd(".././main")
##################################################################
########################### Naive Base ###########################
##################################################################
base_method = "naive"; source('base_forecasts.R') # This script creates the base forecasts and took 18sec

# Main results
ML_method = "randomforest"; source('ML-foreco-main-results.R')  # This script runs the ML forecast reconciliation and took 39min
ML_method = "xgboost"; source('ML-foreco-main-results.R')  # This script took 17min
ML_method = "lightgbm"; source('ML-foreco-main-results.R')  # This script took 29min

################################################################
########################### ETS Base ###########################
################################################################
base_method = "ets"; source('base_forecasts.R') # This script took 75min

# Main results
ML_method = "randomforest"; source('ML-foreco-main-results.R')  # This script took 37min
ML_method = "xgboost"; source('ML-foreco-main-results.R')  # This script took 17min
ML_method = "lightgbm"; source('ML-foreco-main-results.R')  # This script took 29min
ML_method = "bottomup"; source('ML-foreco-main-results.R') 

FoReco_method = "tcs"; source('linear-foreco.R') # This script runs the linear forecast reconciliation 
FoReco_method = "cst"; source('linear-foreco.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco.R')
FoReco_method = "oct"; source('linear-foreco.R')


###################################################################
########################### Sarima Base ###########################
###################################################################
base_method = "sarima"; source('base_forecasts.R') # This script took 262min

# Main results
ML_method = "randomforest"; source('ML-foreco-main-results.R')  # This script took 37min
ML_method = "xgboost"; source('ML-foreco-main-results.R')  # This script took 16min
ML_method = "lightgbm"; source('ML-foreco-main-results.R')  # This script took 28min
ML_method = "bottomup"; source('ML-foreco-main-results.R') 

FoReco_method = "tcs"; source('linear-foreco.R')
FoReco_method = "cst"; source('linear-foreco.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco.R')
FoReco_method = "oct"; source('linear-foreco.R')


#################################################################################
########################### Forecast Combination Base ###########################
#################################################################################
base_method = "averageBF"; source('base_forecasts_combinations.R') 

# Main results
ML_method = "randomforest"; source('ML-foreco-main-results.R')  # This script took 38min
ML_method = "xgboost"; source('ML-foreco-main-results.R')  # This script took 17min
ML_method = "lightgbm"; source('ML-foreco-main-results.R')  # This script took 28min
ML_method = "bottomup"; source('ML-foreco-main-results.R')  

FoReco_method = "tcs"; source('linear-foreco.R')
FoReco_method = "cst"; source('linear-foreco.R')
FoReco_method = "ite"; start_ite = "hts"; source('linear-foreco.R')
FoReco_method = "oct"; source('linear-foreco.R')


###############################################################
########################### Results ###########################
###############################################################
# WAPE
source('Tables_6-7.R'); round(Table6, 4); round(Table7, 4)
# MASE
source("insample_naive_residuals.R") 
source('Tables_A3-A4.R'); round(TableA3, 4); round(TableA4,4)

save(Table6, file = "Table6.RData")
save(Table7, file = "Table7.RData")
save(TableA3, file = "TableA3.RData")
save(TableA4, file = "TableA4.RData")

