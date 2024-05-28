# Script to get the insample naive residuals for MASE

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Load Data ####
ncols_info <- 5 # number of columns that contain info
zone_selected = "NYC" # select the zone (capital first letter)

base_method = "naive" #ets, #sarima, naive
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
resid_save_insample <- vector(mode = "list", length = n_ts)
names(resid_save_insample) <- paste0("m",ts_aggr_orders)
for(ts in 1:n_ts){
  miter <- ts_aggr_orders[ts]
  empty_mat <- matrix(NA, nrow = window_hours/miter + validation/miter - Whour/miter, ncol = length(rolling_sequence))
  colnames(empty_mat) <- c(paste("rolling window", 1:length(rolling_sequence)))
  resid_save_insample[[ts]] <- empty_mat
}

insample_naive_residuals = rep(list(resid_save_insample), n_cs) 
names(insample_naive_residuals) = names_cs


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
  
  ## Prepare temporal DATA (split according to training, validation, insample (training+validation), oos)
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
  

  ######################################################
  ##---------- Forecasts for TEST (oos) set ----------##
  ######################################################
  if(base_method == "naive"){
    forecasts_oos <- fit_base_forecast_oos(x = demand_window_insample, base_method = "naive", 
                                           ts_aggr_orders = ts_aggr_orders, 
                                           Whour = Whour, horizon = horizon, oos_indices_fix = oos_indices_fix,
                                           n_cs = n_cs, names_cs = names_cs)
  }
  residuals_insample_iwindow <- forecasts_oos$residuals_insample
  
  idx = which(iwindow == rolling_sequence)
  # save base forecasts in our object form
  for(ts in 1:n_ts){
    miter = ts_aggr_orders[ts]
    for(csl in 1:n_cs){
      ts_extract_insample <- residuals_insample_iwindow[[ts]][,csl]
      insample_naive_residuals[[csl]][[ts]][,idx] <- ts_extract_insample
    }
  }
  ptme = proc.time()-ptm
}
ptme_total = proc.time() - ptm_total


dir.create(file.path(".",zone_selected), recursive = TRUE, showWarnings = FALSE)
setwd(paste0("./", zone_selected))

save(insample_naive_residuals, file = "insample_naive_residuals.RData")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
