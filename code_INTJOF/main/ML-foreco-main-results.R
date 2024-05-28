# ML forecast reconcilation script

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

set.seed(2023)
#### Load Data ####
ncols_info <- 5 # number of columns that contain info
zone_selected = "NYC" # select the zone (capital first letter)

### Base method parameters ###
# base_method = "ets" #ets, #sarima, naive
fourier_dummy = FALSE # only for ets and sarima (FALSE = standard)
optimizeK = TRUE # only for ets and sarima 

### Machine Learning method parameters ###
# ML_method = "randomforest" #options: bottomup, xgboost, lightgbm, randomforest 
ML_loss = "squared" #options: squared, tweedie (only for xgboost and lightgbm)
ML_predictor = "cstemp" #cs #temp (predictors of the machine learning model: cross-temporal, cross-sectional, temporal)
#tweedie_variance_power = 1.5 # only for xgboost and light gbm
round_base = TRUE # round base forecasts before Ml reconciliation 

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
if(base_method == "naive" | base_method == "averageBF"){
  base_method_name = base_method
}

inputlist_ML_base_forecasts <- get(load(paste0(base_method_name, "_inputlist_ML_base_forecasts.RData")))
rm(list = paste0(base_method_name, "_inputlist_ML_base_forecasts"))

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


##### OBJECTS TO SAVE RESULTS ####
# oos base forecasts
time_index_oos <- area_data[-c(1:(window_hours+validation)),1]
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

recon_forecasts_oos= rep(list(fct_save_oos), n_cs) 
names(recon_forecasts_oos) = names_cs

if(ML_method == "lightgbm" | ML_method == "xgboost" ){
  if(ML_loss == "tweedie"){
    ML_name = paste0(base_method_name, "_", ML_method, "_tweedie", tweedie_variance_power,"_",ML_predictor)
  }else{
    ML_name = paste0(base_method_name, "_", ML_method, "_squared_",ML_predictor)
  }
}else{
  ML_name = paste0(base_method_name, "_", ML_method,"_",ML_predictor)
}

seed_mat = matrix( sample(1:1000000, length(rolling_sequence)*n_cs_bt, replace = F), nrow = length(rolling_sequence), ncol = n_cs_bt)
colnames(seed_mat) = delivery_areas

ptm_total = proc.time()
for(iwindow in rolling_sequence){
  ptm = proc.time()
  #### Prepare Data ####
  demand_window_full <- area_data[(iwindow - window_hours+1):(iwindow + validation + horizon),-c(1:ncols_info)]
  # temporal aggregation 
  demand_hierarchy <- aggregate_ts_hierachy(demand_window_full, ts_aggr_orders) # list with elements decreasing in frequency list[[1]] = highest_frequency (30-min),..., list[[n_ts]] = lowest_frequency (weekly)
  
  ## Prepare temporal DATA (split according to training, validation, insample (training+validation), oos)
  demand_window_oos <- vector(mode = "list", length = n_ts)
  names(demand_window_oos) <- paste0("m",ts_aggr_orders)
  
  oos_indices <- vector(mode = "list", length = n_ts)
  
  for(ts in 1:n_ts){
    demand_full <- demand_hierarchy[[ts]]
    miter <- ts_aggr_orders[ts]
    demand_window_oos[[ts]] <- demand_full[(window_hours/miter+validation/miter+1):(window_hours/miter+validation/miter+horizon/miter),,drop = F]
    
    # Rolling window indices for each temporal aggregation
    oos_indices[[ts]] <- (iwindow/miter+validation/miter+1): (iwindow/miter+ validation/miter + horizon/miter) # out-of-sample
  }
  
  #################################################
  ## ------------ ML reconcilation --------------##
  #################################################
  idx = which(iwindow == rolling_sequence)
  
  # apply ML to highest frequency (30min), bottom-level (delivery-area) forecasts (for each delivery area)
  # No tuning of hyperparameters
  ml_reconcile <- fit_ml_reconciliation(inputlist_ML_base_forecasts = inputlist_ML_base_forecasts[[idx]], n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, C = C, 
                                        ts_aggr_orders = ts_aggr_orders, delivery_areas = delivery_areas, 
                                        ML_method = ML_method, 
                                        ML_loss = ML_loss, ML_predictor = ML_predictor, # tweedie_variance_power = 1.5, 
                                        round_base = round_base, 
                                        seed_mat = seed_mat[idx,])
  recon_csts_forecasts <- ml_reconcile
  
  # save reconciled forecasts for test set
  for(ts in 1:n_ts){
    miter = ts_aggr_orders[ts]
    for(csl in 1:n_cs){
      ts_extract_fcst <- recon_csts_forecasts[[ts]][,csl]
      ts_extract_actual <- demand_window_oos[[ts]][,csl]
      
      # actuals
      if(Dhour/miter >= 1){
        # actual
        recon_forecasts_oos[[csl]][[ts]][oos_indices[[ts]] - window_hours/miter - validation/miter,1] <- ts_extract_actual
        
        #forecasts
        for(hday in 1:horizonDays){
          get_indices = oos_indices[[ts]][((hday-1)*Dhour/miter + 1) : (hday*Dhour/miter)]
          get_col <- hday + 1 # always start with second column?
          recon_forecasts_oos[[csl]][[ts]][get_indices - window_hours/miter - validation/miter, get_col] = ts_extract_fcst[((hday-1)*Dhour/miter + 1) : (hday*Dhour/miter)]
        }
      }else{
        ts_prev = which((Dhour/ts_aggr_orders) == 1)
        miter_prev = ts_aggr_orders[ts_prev]
        #actual
        recon_forecasts_oos[[csl]][[ts]][oos_indices[[ts_prev]][1] - window_hours/miter_prev - validation/(miter_prev),1] <- ts_extract_actual
        #forecast
        recon_forecasts_oos[[csl]][[ts]][oos_indices[[ts_prev]][1] - window_hours/miter_prev - validation/(miter_prev),2] <- ts_extract_fcst
      }
    }
  }
  
  ptme = proc.time()-ptm
}
ptme_total = proc.time() - ptm_total

assign(paste0(ML_name,"_recon_forecasts_oos"), recon_forecasts_oos)
objectname_oos <- paste0(ML_name,"_recon_forecasts_oos")
save(list = objectname_oos, file = paste0(objectname_oos,".RData"))

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))