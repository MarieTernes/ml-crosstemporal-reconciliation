#####################################################################################################################
#-------------------- Example script  ------------------#
# "Cross-Temporal Forecast Reconciliation at Digital Platforms with Machine Learning" by Jeroen Rombouts, Marie Ternes and Ines Wilms #
# arXiv link: https://arxiv.org/abs/2402.09033
#######################################################################################################################################

rm(list=ls())
# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Libraries ####
library(forecast)
library(randomForest)
library(tidyverse)
library(janitor)
library(caret)
library(xgboost)
library(FoReco)
library(fastDummies)
library(lightgbm)
library(rBayesianOptimization)
library(mlr)
source("functions.R")


load("data_example.RData")
head(data_example)


################################################
################################################
##---------------- Part 0 --------------------##
## --- Setting parameters / prerequisites --- ##
################################################
################################################

# Cross-sectional aggregation levels
names_cs <- c( "Market", "Z1", "Z2", "A1", "A2", "A3", "A4", "A5") # names of all cross-sectional units (Market, zones, delivery areas)
zones <- c( "Z1", "Z2")
delivery_areas <- c( "A1", "A2", "A3", "A4", "A5")
C <- rbind(rep(1,5), c(1,1,1,0,0), c(0,0,0,1,1)) # cross-sectional matrix mapping the bottom level series into the higher level ones
rownames(C) <- c( "Market", "Z1", "Z2")
colnames(C) <- c("A1", "A2", "A3", "A4", "A5")
n_cs_level <- c(1,2,5) # number of cross-sectional units for each cross-sectional level
n_cs <- sum(n_cs_level) # total number of all cross-sectional units
n_cs_bt <- 5 # number of bottom-level cs units (delivery areas)
n_cs_up <-  n_cs - n_cs_bt # number of upper-level cs units (market + zones)

# Temporal aggregation level
mbt = 1 # bottom-level (30 min) 
m2 = 2 # hourly
m1 = 34 # top-level (daily)
ts_aggr_orders = c(mbt,m2,m1)
n_ts = length(ts_aggr_orders) # number of different temporal frequencies

# Settings forecast exercise #
window_days <- 5*28 # number of days in training sample (140 days) # needs to be divisible by 7! 
Dhour <- 34 # number of time slots per day (for 30-min data 34, 8am - 11:30pm)
Dwday <- 7 # Number of days a week
Whour <- Dhour*Dwday # number of time slots per week 
window_hours <- window_days*Dhour # First training set is based on window_hours
train_hours <- (window_days + Dwday*3) * Dhour # Last training set 
validationDays <- Dwday*4 # number of days in validation set, four full weeks ahead
validation <- Dhour*validationDays # length validation set, four full weeks ahead
rolling_validation <- seq(from = window_hours, by = Whour, to = train_hours) # All training sets for validation sample
horizonVal <- validation/4 # forecast horizon for validation set, one full week ahead
horizonDays <-7 # number of days of forecast horizon 
horizon <- Dhour*horizonDays # forecast horizon, one full week ahead

# Seasonality
seasonal_frequency_30min <- Dhour # daily seasonal frequency for SARIMA and ETS
seasonal_frequency_hourly <- Dhour/2 # daily seasonal freq for SARIMA and ETS
seasonal_frequency_daily <- Dwday # weekly seasonal freq for SARIMA and ETS
seasonal_frequency <- c(seasonal_frequency_30min, seasonal_frequency_hourly, seasonal_frequency_daily)
Kfourier = floor(seasonal_frequency/2)

# set inner rolling window indices
val_indices_fix <- oos_indices_fix <- vector(mode = "list", length = n_ts)
for(ts in 1:n_ts){
  miter = ts_aggr_orders[ts]
  val_indices_fix[[ts]] <- (window_hours/miter+1):(window_hours/miter + validation/miter) # validation
  oos_indices_fix[[ts]] <- (window_hours/miter+validation/miter+1): (window_hours/miter+ validation/miter + horizon/miter) # out-of-sample
}


################################################
################################################
##---------------- Part 1 --------------------##
## ------ Getting the base forecasts ---------##
################################################
################################################
# Parameters:
base_method = "naive" # options: ets, sarima, naive
fourier_dummy = FALSE # only for ets and sarima 
optimizeK = TRUE # only for ets and sarima 

# Data: temporal aggregation from highest frequency (30 min) to hourly and daily frequency
demand_hierarchy <- aggregate_ts_hierachy(data_example, ts_aggr_orders) # list with elements decreasing in frequency list[[1]] = highest_frequency (30-min),..., list[[n_ts]] = lowest_frequency (weekly)


## Prepare temporal DATA (split according to training, validation, insample (training+validation), oos) 
## for the time-series rolling window procedure to get the base forecasts
# See Figure 4 of the corresponding paper for visualization 
# training: estimate base forecast model of your choice (naive, ets, sarima)
# validation: use base forecast model from training set and forecast for validation period, use these base forecasts and the actuals to train ML model  
# insample: estimate base forecast model of your choice (naive, ets, sarima)
# out-of-sample (oos): use base forecast model from insample set and forecast to for out-of-sample period, use these base forecast and previously estimated ML 
# to reconcile the base forecast (and then evaluate out of sample forecast performance of reconciled forecasts) 
demand_window_train <- demand_window_val <- demand_window_insample <- demand_window_oos <- vector(mode = "list", length = n_ts)
names(demand_window_train) <- names(demand_window_val) <- names(demand_window_insample) <- names(demand_window_oos) <- paste0("m",ts_aggr_orders)

for(ts in 1:n_ts){
  demand_full <- demand_hierarchy[[ts]]
  miter <- ts_aggr_orders[ts]
  demand_window_train[[ts]] <- demand_full[1:(train_hours/miter),]
  demand_window_val[[ts]] <- demand_full[(window_hours/miter+1):(window_hours/miter+validation/miter),]
  demand_window_insample[[ts]] <- demand_full[1:(window_hours/miter+validation/miter),]
  demand_window_oos[[ts]] <- demand_full[(window_hours/miter+validation/miter+1):(window_hours/miter+validation/miter+horizon/miter),,drop = F]
}

#################################################
## ----- Forecasts for VALIDATION set ---------##
#################################################
# Remark about parameter: iwindow_first = TRUE (default)
# can be set to FALSE if doing a rolling window forecast exercise after the first rolling window
# As beyond iteration r = 1, we can re-use the base forecasts for the first three weeks from the previous iteration 
# and need to fit the forecasting model only once to obtain the base forecasts for the fourth week to construct the validation set
forecasts_val_naive = fit_base_forecast_validation(x = demand_window_train, base_method = "naive", 
                                                   ts_aggr_orders = ts_aggr_orders, 
                                                   window_hours = window_hours, Whour = Whour, validation = validation, horizonVal = horizonVal, val_indices_fix = val_indices_fix, rolling_validation = rolling_validation, 
                                                   n_cs = n_cs, names_cs = names_cs,
                                                   iwindow_first = TRUE)
forecasts_val_ets = fit_base_forecast_validation(x = demand_window_train, base_method = "ets", 
                                                 ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                                 window_hours = window_hours, Whour = Whour, validation = validation, horizonVal = horizonVal, val_indices_fix = val_indices_fix, rolling_validation = rolling_validation, 
                                                 n_cs = n_cs, names_cs = names_cs,  
                                                 fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = Kfourier,
                                                 iwindow_first = TRUE)
forecasts_val_sarima = fit_base_forecast_validation(x = demand_window_train, base_method = "sarima", 
                                                    ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                                    window_hours = window_hours, Whour = Whour, validation = validation, horizonVal = horizonVal, val_indices_fix = val_indices_fix, rolling_validation = rolling_validation, 
                                                    n_cs = n_cs, names_cs = names_cs,
                                                    fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = Kfourier,
                                                    iwindow_first = TRUE)

forecasts_val = forecasts_val_ets # or: forecasts_val_naive, forecasts_val_sarima



#################################################
##------- Forecasts for TEST (oos) set --------##
#################################################
base_forecasts_oos_naive = fit_base_forecast_oos(x = demand_window_insample, base_method = "naive", 
                                                 ts_aggr_orders = ts_aggr_orders, 
                                                 window_hours = window_hours, Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix,
                                                 n_cs = n_cs, names_cs = names_cs)
base_forecasts_oos_ets = fit_base_forecast_oos(x = demand_window_insample, base_method = "ets", 
                                               ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                               window_hours = window_hours, Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix, 
                                               n_cs = n_cs, names_cs = names_cs,
                                               fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = Kfourier)
base_forecasts_oos_sarima = fit_base_forecast_oos(x = demand_window_insample, base_method = "sarima", 
                                                  ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                                  window_hours = window_hours, Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix, 
                                                  n_cs = n_cs, names_cs = names_cs,
                                                  fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = Kfourier)

base_forecasts_oos = base_forecasts_oos_ets$base_forecasts_oos # or: base_forecasts_oos_naive$base_forecasts_oos, base_forecasts_oos_sarima$base_forecasts_oos

#################################################
##----------- Save base forecasts  ------------##
# ------- For validation and test set ----------#
#################################################
# See Section 3.2 of paper for construction of y, Xin and Xout of ML model
# necessary for ML model #
inputlist_ML_base_forecasts <- list()
# Y
inputlist_ML_base_forecasts$Y = demand_window_val[[1]]

# Xin and Xout
X_val = c()
X_oos = c()
for(ts in 1:n_ts){
  miter = ts_aggr_orders[ts]
  X_ts_val = apply(forecasts_val[[ts]], 2, rep, each = miter)
  X_ts_oos = apply(base_forecasts_oos[[ts]], 2, rep, each = miter)
  X_val = cbind(X_val, X_ts_val)
  X_oos = cbind(X_oos, X_ts_oos)
}
X_val = pmax(X_val,0)
X_oos = pmax(X_oos,0)
colnames(X_val) <- colnames(X_oos) <- paste0(rep(names_cs, n_ts), rep(paste0("_m", ts_aggr_orders), each = n_cs))
inputlist_ML_base_forecasts$Xin = X_val
inputlist_ML_base_forecasts$Xout = X_oos


################################################
################################################
##---------------- Part 2 --------------------##
## ------ Machine Learning reconciliation ----##
################################################
################################################
# Parameters
ML_method = "randomforest" #options: bottomup, xgboost, lightgbm, randomforest 
ML_loss = "squared" # options: squared, #tweedie (only for xgboost and lightgbm)
ML_predictor = "cstemp" #cs #temp (predictors of the machine learning model: cross-temporal, cross-sectional, temporal)
tweedie_variance_power = 1.5 # only for xgboost and lightgbm
round_base = TRUE # round base forecasts before Ml reconciliation 

# No tuning of hyperparameters
ml_reconcile = fit_ml_reconciliation(inputlist_ML_base_forecasts = inputlist_ML_base_forecasts, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, C = C, 
                                     ts_aggr_orders = ts_aggr_orders, delivery_areas = delivery_areas, 
                                     ML_method = "xgboost", 
                                     ML_loss = "squared", ML_predictor = "cstemp", # tweedie_variance_power = 1.5, 
                                     round_base = TRUE, 
                                     seed = 2023)

mapply(wape, actual = as.data.frame(demand_window_oos$m1), forecast = as.data.frame(ml_reconcile$m1))
mapply(wape, actual = as.data.frame(demand_window_oos$m2), forecast = as.data.frame(ml_reconcile$m2))
mapply(wape, actual = as.data.frame(demand_window_oos$m34), forecast = as.data.frame(ml_reconcile$m34))

# Tuning of hyperparameters
ml_reconcile_tuned = fit_ml_reconciliation_tuning(inputlist_ML_base_forecasts = inputlist_ML_base_forecasts, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, C = C, 
                                                  ts_aggr_orders = ts_aggr_orders, delivery_areas = delivery_areas, 
                                                  ML_method = "xgboost", 
                                                  ML_loss = "squared", ML_predictor = "cstemp", round_base = TRUE,
                                                  seed = 2023)

mapply(wape, actual = as.data.frame(demand_window_oos$m1), forecast = as.data.frame(ml_reconcile_tuned$recon_csts_forecasts$m1))
mapply(wape, actual = as.data.frame(demand_window_oos$m2), forecast = as.data.frame(ml_reconcile_tuned$recon_csts_forecasts$m2))
mapply(wape, actual = as.data.frame(demand_window_oos$m34), forecast = as.data.frame(ml_reconcile_tuned$recon_csts_forecasts$m34))

ml_reconcile_tuned$params_best # tuned parameters



