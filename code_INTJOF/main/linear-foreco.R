# Linear forecast reconcilition script

# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Load Data ####
ncols_info <- 5 # number of columns that contain info
zone_selected = "NYC" # select the zone (capital first letter)

### Base method parameters ###
# base_method = "ets" #ets, #sarima, naive
fourier_dummy = FALSE # only for ets and sarima 
optimizeK = TRUE # only for ets and sarima 

### FoReco parameters ###
# FoReco_method = "cst" # tcs, cst, ite, oct, ctbu
thf_comb = "wlsv" # Type of covariance matrix to be used in the temporal reconciliation (kstar x m) x (kstar x m): bu, ols, struc, wls, wlsh, acov, strar1, sar1, har1, shr, sam, omega
hts_comb = "shr" # Type of covariance matrix to be used in the cross-sectional reconciliation (n x n): bu, ols, struc, wls, shr, sam, w
csts_comb = "wlsv"  # Type of covariance matrix to be used in the cross-temporal reconciliation n(kstar x m) x n(kstar x m):
# start_ite = "thf" # relevant for "ite": dimension along with the first reconciliation step in each iteration is performed: 
#                   #                     start from temporal reconciliation "thf" (default) or from cross-sectional with "hts"
round_basef = FALSE

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
if(base_method == "naive"| base_method == "averageBF" ){
  base_method_name = base_method
}

inputlist_FoReco_package <- get(load(paste0(base_method_name, "_inputlist_FoReco_package.RData")))
rm(list = paste0(base_method_name, "_inputlist_FoReco_package"))

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


##### OBJECTS TO SAVE RESULTS
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

recon_forecasts_oos = rep(list(fct_save_oos), n_cs) 
names(recon_forecasts_oos) = names_cs

if(FoReco_method == "tcs"| FoReco_method == "cst"){
  FoReco_method_name = paste0(FoReco_method, "_t-", thf_comb, "_cs-", hts_comb)
}
if(FoReco_method == "ite"){
  FoReco_method_name = paste0(FoReco_method, "_t-", thf_comb, "_cs-", hts_comb, "_", start_ite)
}
if(FoReco_method == "oct"){
  FoReco_method_name = paste(FoReco_method, csts_comb, sep = "_")
}
if(FoReco_method == "ctbu"){
  FoReco_method_name = FoReco_method
}

ptm_total = proc.time()
for(iwindow in rolling_sequence){
  ptm = proc.time()
  #### Prepare Data ####
  demand_window_full <- area_data[(iwindow - window_hours+1):(iwindow + validation + horizon),-c(1:ncols_info)]
  # Data: temporal aggregation from highest frequency (30 min) to lowest frequency
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
  ## ------------ FoReco reconcilation ----------##
  #################################################
  idx = which(iwindow == rolling_sequence)
  basef = inputlist_FoReco_package[[idx]]$base
  resids = inputlist_FoReco_package[[idx]]$residuals
  
  if(FoReco_method == "tcs"){
    tcs_recf <- tcsrec(basef, m = rev(ts_aggr_orders), C = C, 
                       thf_comb = thf_comb, hts_comb = hts_comb,
                       res = resids)$recf
    recf <- tcs_recf
  }
  if(FoReco_method == "cst"){
    cst_recf <- cstrec(basef, m = rev(ts_aggr_orders), C = C,
                       thf_comb = thf_comb, hts_comb = hts_comb,
                       res = resids)$recf
    recf <-  cst_recf
  }
  if(FoReco_method == "ite"){
    ite_recf <- iterec(basef, note = FALSE, 
                       m = rev(ts_aggr_orders), C = C,
                       thf_comb = thf_comb, hts_comb = hts_comb,
                       res = resids, start_rec = start_ite)$recf
    recf <-  ite_recf
  }
  if(FoReco_method == "oct"){
    oct_recf <- octrec(basef, m = rev(ts_aggr_orders), C = C,
                       comb = csts_comb, res = resids, keep = "recf")
    recf <-  oct_recf
  }
  if(FoReco_method == "ctbu"){
    h <- horizon/ts_aggr_orders[n_ts]
    basef = inputlist_FoReco_package[[idx]]$base[-c(1:n_cs_up), -c(1:(ks*h))]
    if(round_basef){
      basef = pmax(round(basef), 0)
    }
    ctbu_recf <- ctbu(Bmat = basef, m = rev(ts_aggr_orders), C = C)
    recf <-  ctbu_recf
  }
  
  # column indices to extract reconciled forecasts per temporal frequency
  start_ts = rev(cumsum(rev(horizon/ts_aggr_orders)) - rev(horizon/ts_aggr_orders) + 1)
  end_ts = rev(cumsum(rev(horizon/ts_aggr_orders)) )
  
  # save reconciled forecasts for test set
  for(ts in 1:n_ts){
    miter = ts_aggr_orders[ts]
    recon_csts_forecasts <- t(recf[,(start_ts[ts]:end_ts[ts]), drop = F]) 
    for(csl in 1:n_cs){
      ts_extract_fcst <- recon_csts_forecasts[,csl]
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
ptme_total = proc.time()-ptm_total

assign(paste0(base_method_name, "_", FoReco_method_name, "_recon_forecasts_oos"), recon_forecasts_oos)
objectname_oos <- paste0(base_method_name, "_", FoReco_method_name, "_recon_forecasts_oos")
save(list = objectname_oos, file = paste0(objectname_oos,".RData"))


# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
