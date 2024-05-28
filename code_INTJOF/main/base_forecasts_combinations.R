# Base forecast script specifically to combine sarima, ets and naive 

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Load Data ####
ncols_info <- 5 # number of columns that contain info
zone_selected = "NYC" # select the zone (capital first letter)

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

# load required base forecasts 
setwd(paste0("./", zone_selected))
base_naive <- get(load(paste0("naive_base_forecasts_oos.RData")))
base_ets <- get(load(paste0("ets_standard_base_forecasts_oos.RData")))
base_sarima <- get(load(paste0("sarima_standard_base_forecasts_oos.RData")))

inputlist_ML_base_forecasts_naive <- get(load(paste0("naive_inputlist_ML_base_forecasts.RData")))
inputlist_ML_base_forecasts_ets <- get(load(paste0("ets_standard_inputlist_ML_base_forecasts.RData")))
inputlist_ML_base_forecasts_sarima <- get(load(paste0("sarima_standard_inputlist_ML_base_forecasts.RData")))

inputlist_FoReco_package_naive <- get(load(paste0("naive_inputlist_FoReco_package.RData")))
inputlist_FoReco_package_ets <- get(load(paste0("ets_standard_inputlist_FoReco_package.RData")))
inputlist_FoReco_package_sarima <- get(load(paste0("sarima_standard_inputlist_FoReco_package.RData")))


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

# equally weighted base forecasts from naive, ets and sarima 
for(cs in 1:n_cs){
  for(ts in 1:n_ts){
    base_list = list(base_naive[[cs]][[ts]], base_ets[[cs]][[ts]], base_sarima[[cs]][[ts]])
    base_forecasts_oos[[cs]][[ts]] = apply(simplify2array(base_list), c(1,2), mean)
  }
}

# Input for FoReco is the average of all base forecasts 
inputlist_FoReco_package <- vector(mode = "list", length = length(rolling_sequence))

start = (cumsum(rev((window_hours + validation)/ts_aggr_orders)) - rev((window_hours + validation)/ts_aggr_orders) + 1)
end = start+Dwday*ts_aggr_orders - 1
dlt_col = c(start[1]:end[1]) #delete residuals because with naive we lost one weeks of observations 
for(ts in 2:n_ts){
  dlt_col = c(dlt_col, start[ts]:end[ts])
}


for(i in 1:length(rolling_sequence)){
  prep_ets_resid = inputlist_FoReco_package_ets[[i]]$residuals[,-dlt_col]
  prep_sarima_resid = inputlist_FoReco_package_sarima[[i]]$residuals[,-dlt_col]
  base_list = list(inputlist_FoReco_package_naive[[i]]$base, inputlist_FoReco_package_ets[[i]]$base, inputlist_FoReco_package_sarima[[i]]$base)
  residuals_list = list(inputlist_FoReco_package_naive[[i]]$residuals, prep_ets_resid, prep_sarima_resid)
  inputlist_FoReco_package[[i]]$base = apply(simplify2array(base_list), c(1,2), mean)
  inputlist_FoReco_package[[i]]$residuals = apply(simplify2array(residuals_list), c(1,2), mean)
}

# Input for ML is the average of all base forecasts 
inputlist_ML_base_forecasts <- vector(mode = "list", length = length(rolling_sequence))

for(i in 1:length(rolling_sequence)){
  inputlist_ML_base_forecasts[[i]]$Y = inputlist_ML_base_forecasts_naive[[i]]$Y
  Xin_list = list(inputlist_ML_base_forecasts_naive[[i]]$Xin, inputlist_ML_base_forecasts_ets[[i]]$Xin, inputlist_ML_base_forecasts_sarima[[i]]$Xin)
  Xout_list = list(inputlist_ML_base_forecasts_naive[[i]]$Xout, inputlist_ML_base_forecasts_ets[[i]]$Xout, inputlist_ML_base_forecasts_sarima[[i]]$Xout)
  inputlist_ML_base_forecasts[[i]]$Xin = apply(simplify2array(Xin_list), c(1,2), mean)
  inputlist_ML_base_forecasts[[i]]$Xout = apply(simplify2array(Xout_list), c(1,2), mean)
}

# save inputs 
assign(paste0("averageBF_inputlist_FoReco_package"), inputlist_FoReco_package)
objectname_FoReco <- paste0("averageBF_inputlist_FoReco_package")
save(list = objectname_FoReco, file = paste0(objectname_FoReco,".RData"))

assign(paste0("averageBF_inputlist_ML_base_forecasts"), inputlist_ML_base_forecasts)
objectname_ML <- paste0("averageBF_inputlist_ML_base_forecasts")
save(list = objectname_ML, file = paste0(objectname_ML,".RData"))

assign(paste0("averageBF_base_forecasts_oos"), base_forecasts_oos)
objectname_oos <- paste0("averageBF_base_forecasts_oos")
save(list = objectname_oos, file = paste0(objectname_oos,".RData"))

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))