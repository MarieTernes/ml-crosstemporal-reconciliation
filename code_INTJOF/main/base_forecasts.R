# Base forecast script

# Set working directory to the folder where this script is contained in 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Load Data ####
ncols_info <- 5 # number of columns that contain info
zone_selected = "NYC" # select the zone (capital first letter)

# base_method = "ets" #ets, #sarima, naive
fourier_dummy = FALSE # for ets and sarima 
optimizeK = TRUE

# Prepare data #
area_data <- citibike$citibike
datetime <- as.POSIXct(paste(area_data$year, area_data$month, area_data$day, area_data$hour, ifelse(area_data$halfhour==0, 0, 30), sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "America/New_York")

# cross-sectional information #
delivery_areas <- paste0("area", 1:6)
names_cs <- colnames(area_data)[-c(1:ncols_info)] # names of all cross-sectional units (Total, catchment areas, delivery_areas)
C <- citibike$Cmatrix # cross-sectional (contemporaneous) matrix mapping the bottom level series into the higher level ones
n_cs_level <- c(1,6) # number of cross-sectional units for each cross-sectional level
n_cs <- sum(n_cs_level) # total number of all cross-sectional-units
n_cs_bt <- 6 # number of bottom-level cs units
n_cs_up <-  n_cs - n_cs_bt # number of upper-level cs units 

# Temporal aggregation 
ts_aggr_orders = c(1,2,3,4,6,8,12,16,24,48) #30-min, 1-hour, 1.5-hour, 2-hour, 3-hour, 4-hour, 6-hour, 8-hour, 12-hour, 24-hour
n_ts = length(ts_aggr_orders) # number of different temporal frequencies

# Seasonality 
seasonal_frequency <- c(48,24,16,12,8,6*7,4*7,3*7,2*7,7) # seasonal frequency for 30-min, 1-hour, 1.5-hour, 2-hour, 3-hour, 4-hour, 6-hour, 8-hour, 12-hour, 24-hour
Kfourier = floor(seasonal_frequency/2)

# Settings forecast exercise 
window_days <- 5*28 # number of days in training sample (140 days) # needs to be divisible by 7! 
Dhour <- 48 # number of time slots per day (for 30-min data 48, 00:00 - 11:30pm)
Dwday <- 7 # Number of days a week
Whour <- Dhour*Dwday # number of time slots per week 
window_hours <- window_days*Dhour # First training set is based on window_hours
train_hours <- (window_days + Dwday*4 - 1) * Dhour # Last training set # (window_days + Dwday*3) * Dhour for H = 7 days
validationDays <- Dwday*4 # number of days in validation set, four full weeks ahead
validation <- Dhour*validationDays # length validation set, four full weeks ahead
horizonVal <- Dhour # forecast horizon for validation set, one full day ahead
horizonDays <- 1 # number of days of forecast horizon 
horizon <- Dhour*horizonDays # forecast horizon, one full week ahead

# set rolling window indices and inner rolling window indices
rolling_sequence <- seq(from = window_hours, by = Dhour, to = nrow(area_data) - horizon - validation) # by = Whour for H = 7 days
rolling_validation <- seq(from = window_hours, by = Dhour, to = train_hours)  # All training sets for validation sample
ival_last = rolling_validation[length(rolling_validation)] # fix to the last one
val_indices_fix <- oos_indices_fix <- vector(mode = "list", length = n_ts)
for(ts in 1:n_ts){
  miter = ts_aggr_orders[ts]
  val_indices_fix[[ts]] <- (window_hours/miter+1):(window_hours/miter + validation/miter) # validation
  oos_indices_fix[[ts]] <- (window_hours/miter+validation/miter+1): (window_hours/miter+ validation/miter + horizon/miter) # out-of-sample
}

##### OBJECTS TO SAVE RESULTS ####
# oos base forecasts
time_index_oos <- datetime[-c(1:(window_hours+validation))]
fct_save_oos <- vector(mode = "list", length = n_ts)
names(fct_save_oos) <- paste0("m",ts_aggr_orders)
for(ts in 1:n_ts){
  miter <- ts_aggr_orders[ts]
  empty_mat <- matrix(NA, nrow = nrow(area_data)/miter - window_hours/miter - validation/miter, ncol = horizonDays + 1)
  index_ts <- seq(from = 1, to = nrow(area_data) - window_hours - validation, by = miter)
  rownames(empty_mat) <- as.character(format(time_index_oos[index_ts], format = "%Y-%m-%d %H:%M"))
  colnames(empty_mat) <- c("Actual", paste("Day-ahead", 1:horizonDays))
  fct_save_oos[[ts]] <- empty_mat
}

base_forecasts_oos = rep(list(fct_save_oos), n_cs) 
names(base_forecasts_oos) = names_cs

# List to save objects from validation set that are needed for ML model and FoReco package 
inputlist_ML_base_forecasts <- inputlist_FoReco_package <- vector(mode = "list", length = length(rolling_sequence))

dir.create(file.path(".",zone_selected), recursive = TRUE, showWarnings = FALSE)
setwd(paste0("./", zone_selected))
if(base_method=="ets"| base_method == "sarima"){
  if(fourier_dummy){
    if(optimizeK){
      base_method_name = paste0(base_method, "_fourier_optK")
    }else{
      base_method_name = paste0(base_method, "_fourier_fixedK")
    }
  }else{
    base_method_name = paste0(base_method, "_standard")
  }
}
if(base_method == "naive"){
  base_method_name = base_method
}

ptm_total = proc.time()
for(iwindow in rolling_sequence){
  ptm = proc.time()
  #### Prepare Data ####
  demand_window_full <- area_data[(iwindow - window_hours+1):(iwindow + validation + horizon),-c(1:ncols_info)]
  # Data: temporal aggregation from highest frequency (30 min) to lowest frequency
  demand_hierarchy <- aggregate_ts_hierachy(demand_window_full, ts_aggr_orders) # list with elements decreasing in frequency list[[1]] = highest_frequency (30-min),..., list[[n_ts]] = lowest_frequency (weekly)
  
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
  
  oos_indices <- vector(mode = "list", length = n_ts)
  
  for(ts in 1:n_ts){
    demand_full <- demand_hierarchy[[ts]]
    miter <- ts_aggr_orders[ts]
    demand_window_train[[ts]] <- demand_full[1:(train_hours/miter),]
    demand_window_val[[ts]] <- demand_full[(window_hours/miter+1):(window_hours/miter+validation/miter),]
    demand_window_insample[[ts]] <- demand_full[1:(window_hours/miter+validation/miter),]
    demand_window_oos[[ts]] <- demand_full[(window_hours/miter+validation/miter+1):(window_hours/miter+validation/miter+horizon/miter),,drop = F]
    
    # Rolling window indices for each temporal aggregation
    oos_indices[[ts]] <- (iwindow/miter+validation/miter+1): (iwindow/miter+ validation/miter + horizon/miter) # out-of-sample
  }
  
  #################################################
  ## ----- Forecasts for VALIDATION set ---------##
  #################################################
  # Remark about parameter: iwindow_first = TRUE (default)
  # can be set to FALSE if doing a rolling window forecast exercise after the first rolling window
  # As beyond iteration r = 1, we can re-use the base forecasts for the first three weeks from the previous iteration 
  # and need to fit the forecasting model only once to obtain the base forecasts for the fourth week to construct the validation set
  if(iwindow == rolling_sequence[1]){
    iwindow_first_flag = TRUE
    forecasts_val_old = NULL
  }else{
    iwindow_first_flag = FALSE
  }
  
  
  if(base_method == "naive"){
    forecasts_val <- fit_base_forecast_validation(x = demand_window_train, base_method = "naive", 
                                                  ts_aggr_orders = ts_aggr_orders, 
                                                  window_hours = window_hours, Whour = Whour, validation = validation, horizonVal = horizonVal, val_indices_fix = val_indices_fix, rolling_validation = rolling_validation, 
                                                  n_cs = n_cs, names_cs = names_cs,
                                                  iwindow_first = iwindow_first_flag, forecasts_val_old = forecasts_val_old)
  }
  
  if(base_method == "ets" | base_method == "sarima"){
    forecasts_val <- fit_base_forecast_validation(x = demand_window_train, base_method = base_method, 
                                                  ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                                  window_hours = window_hours, Whour = Whour, validation = validation, horizonVal = horizonVal, val_indices_fix = val_indices_fix, rolling_validation = rolling_validation, 
                                                  n_cs = n_cs, names_cs = names_cs,  
                                                  fourier_dummy = fourier_dummy, optimizeK = optimizeK, Kfourier = Kfourier,
                                                  iwindow_first = iwindow_first_flag, forecasts_val_old = forecasts_val_old)

  }
  
  
  ######################################################
  ##---------- Forecasts for TEST (oos) set ----------##
  ######################################################
  if(base_method == "naive"){
    forecasts_oos <- fit_base_forecast_oos(x = demand_window_insample, base_method = "naive", 
                                           ts_aggr_orders = ts_aggr_orders, 
                                           Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix,
                                           n_cs = n_cs, names_cs = names_cs)
  }
  
  if(base_method == "ets" | base_method == "sarima"){
    forecasts_oos <- fit_base_forecast_oos(x = demand_window_insample, base_method = base_method, 
                                           ts_aggr_orders = ts_aggr_orders, seasonal_frequency = seasonal_frequency,
                                           Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix, 
                                           n_cs = n_cs, names_cs = names_cs,
                                           fourier_dummy = fourier_dummy, optimizeK = optimizeK, Kfourier = Kfourier)
  }
  base_forecasts_oos_iwindow <- forecasts_oos$base_forecasts_oos
  residuals_insample_iwindow <- forecasts_oos$residuals_insample
  
  #################################################
  ##----------- Save base forecasts  ------------##
  # ------- For validation and test set ----------#
  #################################################
  
  ### ML model
  # See Section 3.2 of paper for construction of y, Xin and Xout of ML model
  # necessary for ML model #
  idx = which(iwindow == rolling_sequence)
  # Y
  inputlist_ML_base_forecasts[[idx]]$Y = demand_window_val[[1]]
  
  # Xin and Xout
  X_val = c()
  X_oos = c()
  for(ts in 1:n_ts){
    miter = ts_aggr_orders[ts]
    X_ts_val = apply(forecasts_val[[ts]], 2, rep, each = miter)
    X_ts_oos = apply(base_forecasts_oos_iwindow[[ts]], 2, rep, each = miter)
    X_val = cbind(X_val, X_ts_val)
    X_oos = cbind(X_oos, X_ts_oos)
  }
  X_val = pmax(X_val,0)
  X_oos = pmax(X_oos,0)
  colnames(X_val) <- colnames(X_oos) <- paste0(rep(names_cs, n_ts), rep(paste0("_m", ts_aggr_orders), each = n_cs))
  inputlist_ML_base_forecasts[[idx]]$Xin = X_val
  inputlist_ML_base_forecasts[[idx]]$Xout = X_oos
  
  # save the forecasts_val for the next iteration for the valdiation set so that we don't need to recalculate all base forecasts
  forecasts_val_old <- forecasts_val
  
  ### FoReco
  # save base forecasts for test set
  # Necessary for FoReco R package 
  # basef forecast n x h(kstar x m)
  # res n x N(kstar x m)
  # test actuals n x h(kstar x m)
  
  basef = t(do.call(rbind, rev(base_forecasts_oos_iwindow)))
  res = t(do.call(rbind, rev(residuals_insample_iwindow)))
  test_actuals = t(do.call(rbind, rev(demand_window_oos)))
  
  h <- horizon/rev(ts_aggr_orders) #horizonDays/Dwday
  colnames(basef) <- colnames(test_actuals) <- paste("m", rep(rev(ts_aggr_orders), h), "_h", do.call("c", as.list(sapply(h, function(x) seq(1:x)))),sep = "")
  
  h <- nrow(residuals_insample_iwindow[[1]])/rev(ts_aggr_orders) 
  colnames(res) <- paste("m", rep(rev(ts_aggr_orders), h), "_h", do.call("c", as.list(sapply(h, function(x) seq(1:x)))),sep = "")
  
  inputlist_FoReco_package[[idx]]$base = basef
  inputlist_FoReco_package[[idx]]$residuals = res
  inputlist_FoReco_package[[idx]]$test_actuals = test_actuals
  
  
  # save base forecasts in our object form
  for(ts in 1:n_ts){
    miter = ts_aggr_orders[ts]
    for(csl in 1:n_cs){
      ts_extract_fcst <- base_forecasts_oos_iwindow[[ts]][,csl]
      ts_extract_actual <-  demand_window_oos[[ts]][,csl]
      
      # actuals
      if(Dhour/miter >= 1){
        # actual
        base_forecasts_oos[[csl]][[ts]][oos_indices[[ts]] - window_hours/miter - validation/miter,1] <- ts_extract_actual
        
        #forecasts
        for(hday in 1:horizonDays){
          get_indices = oos_indices[[ts]][((hday-1)*Dhour/miter + 1) : (hday*Dhour/miter)]
          get_col <- hday + 1 # always start with second column?
          base_forecasts_oos[[csl]][[ts]][get_indices - window_hours/miter - validation/miter, get_col] = ts_extract_fcst[((hday-1)*Dhour/miter + 1) : (hday*Dhour/miter)]
        }
      }else{
        ts_prev = which((Dhour/ts_aggr_orders) == 1)
        miter_prev = ts_aggr_orders[ts_prev]
        #actual
        base_forecasts_oos[[csl]][[ts]][oos_indices[[ts_prev]][1] - window_hours/miter_prev - validation/(miter_prev),1] <- ts_extract_actual
        #forecast
        base_forecasts_oos[[csl]][[ts]][oos_indices[[ts_prev]][1] - window_hours/miter_prev - validation/(miter_prev),2] <- ts_extract_fcst
      }
    }
  }
  ptme = proc.time()-ptm
}
ptme_total = proc.time() - ptm_total

#######

assign(paste0(base_method_name, "_inputlist_FoReco_package"), inputlist_FoReco_package)
objectname_FoReco <- paste0(base_method_name, "_inputlist_FoReco_package")
save(list = objectname_FoReco, file = paste0(objectname_FoReco,".RData"))

assign(paste0(base_method_name, "_inputlist_ML_base_forecasts"), inputlist_ML_base_forecasts)
objectname_ML <- paste0(base_method_name, "_inputlist_ML_base_forecasts")
save(list = objectname_ML, file = paste0(objectname_ML,".RData"))

assign(paste0(base_method_name, "_base_forecasts_oos"), base_forecasts_oos)
objectname_oos <- paste0(base_method_name, "_base_forecasts_oos")
save(list = objectname_oos, file = paste0(objectname_oos,".RData"))


# Set working directory to the folder where this script is contained in 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
