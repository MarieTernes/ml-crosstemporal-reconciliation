metric_per_cs_unit <- function(my_object, horizonDays = 1, metric_choice = "wape"){
  n_ts = length(my_object)
  names_ts = names(my_object)
  empty_mat = matrix(NA, nrow = 1, ncol = horizonDays) # change nrow to be more flexible (for instance over days and not entire oos)
  colnames(empty_mat) <- c(paste("Day-ahead", 1:horizonDays))
  empty_mat_w = NA 
  metric = rep(list(empty_mat), n_ts) 
  names(metric) <- names_ts
  
  for(ts in 1:n_ts){
    my_object_ts = my_object[[ts]]
    for(hday in 1:horizonDays){
      get_col = hday + 1
      if(metric_choice == "wape"){
        metric[[ts]][,hday] = wape(actual = my_object_ts[,1], forecast =  my_object_ts[,get_col]) 
      }
      if(metric_choice == "smape_between_0_100"){
        metric[[ts]][,hday] = SMAPE_between_0_100(actual = my_object_ts[,1], forecast =  my_object_ts[,get_col]) 
      }
    }
  }
  return(metric)
}


access_ts_list <- function(my_object, ts_index = 1, hday_index = 1){
  vec <-  my_object[[ts_index]]
  if(is.numeric(vec)){
    out = vec[hday_index]
  }else{
    out = vec[,hday_index]
  }
  return(out)
}

combine_columns_rmNAs <- function(forecast_object, ts_aggr_orders){
  n_ts = length(ts_aggr_orders)
  forecasts <- vector(mode = "list", length = n_ts)
  names(forecasts) <- paste0("m",ts_aggr_orders)
  
  for(ts in 1:n_ts){
    fcst_new = rowSums(forecast_object[[ts]][,-1, drop = F], na.rm = T)
    out = cbind("Actual" = forecast_object[[ts]][,1], "Day-ahead 1" = fcst_new)
    forecasts[[ts]] = out
  }
  return(forecasts)
}


wape_summary_upd <- function(forecast_object, n_cs, n_cs_up, n_cs_bt, cs_levels = 2, ts_aggr_orders = c(1,17,34), hday_index = 1,  horizonDays = 7,
                             combine_columns_rmNAs_flag = FALSE){
  n_ts = length(ts_aggr_orders)
  hdays = length(hday_index)
  
  # for the week-ahead setting
  if(combine_columns_rmNAs_flag){
    forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
    horizonDays = 1
    hday_index = 1
    hdays = length(hday_index)
  }
  
  wapes <- lapply(forecast_object, metric_per_cs_unit, horizonDays = horizonDays, metric_choice = "wape")
  
  wapes_ts <- vector(mode = "list", length = n_ts)
  for(ts in 1:n_ts){
    wapes_ts[[ts]] = unlist(lapply(wapes, access_ts_list, ts_index = ts, hday_index = hday_index))
  }
  
  # Manchester
  if(cs_levels == 2){
    zone_index = 1:hdays
    da_index = (hdays+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"),paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    wape_summary = matrix(rep(NA, n_ts*cs_levels), ncol = 1)
    rownames(wape_summary) <- lab
    for(ts in 1:n_ts){
      wape_summary[ts] = mean(wapes_ts[[ts]][da_index])  
      wape_summary[n_ts + ts] = mean(wapes_ts[[ts]][zone_index])
    }
  }
  
  # London
  if(cs_levels == 3){
    zone_index = 1:hdays
    catch_index = (hdays+1):(hdays*n_cs_up)
    da_index = (hdays*n_cs_up+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"), paste0("m",ts_aggr_orders)), paste(c("Zone"), paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    
    wape_summary = matrix(rep(NA, n_ts*cs_levels), ncol = 1)
    rownames(wape_summary) <- lab
    for(ts in 1:n_ts){
      wape_summary[ts] = mean(wapes_ts[[ts]][da_index])  
      wape_summary[n_ts + ts] = mean(wapes_ts[[ts]][catch_index])  
      wape_summary[n_ts*2 + ts] = mean(wapes_ts[[ts]][zone_index])
    }
  }
  return(wape_summary)
}


wapes_per_ts_upd <- function(forecast_object, ts_aggr_orders, hday_index, horizonDays, combine_columns_rmNAs_flag = FALSE){
  n_ts = length(ts_aggr_orders)
  # for the week-ahead setting
  if(combine_columns_rmNAs_flag){
    forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
    horizonDays = 1
    hday_index = 1
  }
  hdays = length(hday_index)
  wapes <- lapply(forecast_object, metric_per_cs_unit, horizonDays = horizonDays, metric_choice = "wape")
  
  wapes_ts <- vector(mode = "list", length = n_ts)
  for(ts in 1:n_ts){
    wapes_ts[[ts]] = unlist(lapply(wapes, access_ts_list, ts_index = ts, hday_index = hday_index))
  }
  return(wapes_ts)
}

subperiod_select <- function(fcst_object, n_ts, date_pre, date_post, period = "before"){
  fcst_sub <- vector(mode = "list", length = n_ts)
  for(ts in 1:n_ts){
    dates = as.POSIXct(rownames(fcst_object[[ts]]), tz = "Europe/London")
    if(period == "before"){
      rows_sub = which(dates < date_pre)
    }else if(period == "during"){
      rows_sub = which(dates >= date_pre & dates <= date_post)
    }else{ #after
      rows_sub = which(dates > date_post)
    }
    fcst_sub[[ts]] <- fcst_object[[ts]][rows_sub,]
  }
  return(fcst_sub)
}

subperiod_select_insample_naive_residuals <- function(naive_insample_resid, n_ts, cols_sub){
  naive_insample_resid_sub <- vector(mode = "list", length = n_ts)
  for(ts in 1:n_ts){
    naive_insample_resid_sub[[ts]] <- naive_insample_resid[[ts]][, cols_sub, drop = F]
  }
  return(naive_insample_resid_sub)
}


wape <- function(actual, forecast){
  forecast <- round(forecast)
  df <- cbind(actual, forecast)
  df <- df[complete.cases(df), ]
  actual_rmNA = df[,1]
  forecast_rmNA = df[,2]
  metric <- sum(abs (actual_rmNA - forecast_rmNA)) / sum (abs(actual_rmNA))
  return(metric)
}


SMAPE_alter <- function(actual, forecast){ 
  forecast <- round(forecast) # we round the forecast such that we also have integer values
  # https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
  # This produces a value between 0-100. You just need to remove the division by 2 in the denominator
  num <- abs(forecast  - actual)
  denom <- (actual + forecast)
  #ratio <- num/denom
  #ratio[which(ratio==Inf)] = NA # Zero actual and forecast
  metric <- sum(num)/sum(denom)
  return(metric)
}

#### SMAPE FUNCTION ####
SMAPE_between_0_100 <- function(actual,forecast){ 
  forecast <- round(forecast)
  # https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
  # This produces a value between 0-100. You just need to remove the division by 2 in the denominator
  num <- abs(forecast  - actual)
  denom <- (abs(actual) + abs(forecast))
  ratio <- num/denom
  ratio[which(ratio==Inf)] = NA # This actually hardly occurs, but better solve it when it occurs
  metric <- mean(num/denom, na.rm = TRUE)*100
  return(metric)
}


rmsse_summary_upd <- function(forecast_object, scale_object, n_cs, n_cs_up, n_cs_bt, cs_levels = 2,
                              horizon, nrwindows, ts_aggr_orders){
  
  # for the week-ahead setting
  #if(combine_columns_rmNAs_flag){
  n_ts = length(ts_aggr_orders)
  forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
  horizonDays = 1
  hday_index = 1
  hdays = 1
  #}
  
  rmsses <- mapply( FUN = scaled_error_per_cs_unit, my_object = forecast_object, scale_object = scale_object, 
                    MoreArgs = list(metric_choice = "rmsse", nrwindows = nrwindows,
                                    horizon = horizon, ts_aggr_orders = ts_aggr_orders)
  )
  
  # Manchester
  if(cs_levels == 2){
    zone_index = 1:hdays
    da_index = (hdays+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"),paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    rmsse_summary = matrix(c(rowMeans(rmsses[,da_index]), rowMeans(rmsses[,zone_index, drop = F])), ncol =1)
    rownames(rmsse_summary) <- lab
    
  }
  
  # London
  if(cs_levels == 3){
    zone_index = 1:hdays
    catch_index = (hdays+1):(hdays*n_cs_up)
    da_index = (hdays*n_cs_up+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"), paste0("m",ts_aggr_orders)), paste(c("Zone"), paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    rmsse_summary = matrix(c(rowMeans(rmsses[,da_index]), rowMeans(rmsses[,catch_index]), rowMeans(rmsses[,zone_index, drop = F])), ncol =1)
    rownames(rmsse_summary) <- lab
  }
  
  return(rmsse_summary)
}

mase_summary_upd <- function(forecast_object, scale_object, n_cs, n_cs_up, n_cs_bt, cs_levels = 2,
                             horizon, nrwindows, ts_aggr_orders){
  
  # for the week-ahead setting
  n_ts = length(ts_aggr_orders)
  forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
  horizonDays = 1
  hday_index = 1
  hdays = 1
  
  mases <- mapply( FUN = scaled_error_per_cs_unit, my_object = forecast_object, scale_object = scale_object, 
                   MoreArgs = list(metric_choice = "mase", nrwindows = nrwindows,
                                   horizon = horizon, ts_aggr_orders = ts_aggr_orders)
  )
  
  # Manchester
  if(cs_levels == 2){
    zone_index = 1:hdays
    da_index = (hdays+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"),paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    mase_summary = matrix(c(rowMeans(mases[,da_index]), rowMeans(mases[,zone_index, drop = F])), ncol =1)
    rownames(mase_summary) <- lab
    
  }
  
  # London
  if(cs_levels == 3){
    zone_index = 1:hdays
    catch_index = (hdays+1):(hdays*n_cs_up)
    da_index = (hdays*n_cs_up+1):(hdays*n_cs)
    
    lab <- c(paste(c("Delivery Area"), paste0("m",ts_aggr_orders)), paste(c("Zone"), paste0("m",ts_aggr_orders)), paste(c("City"), paste0("m",ts_aggr_orders)))
    
    mase_summary = matrix(c(rowMeans(mases[,da_index]), rowMeans(mases[,catch_index]), rowMeans(mases[,zone_index, drop = F])), ncol =1)
    rownames(mase_summary) <- lab
  }
  
  return(mase_summary)
}


mases_per_ts <- function(forecast_object, scale_object, horizon, nrwindows, ts_aggr_orders){
  
  # for the week-ahead setting
  n_ts = length(ts_aggr_orders)
  forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
  horizonDays = 1
  hday_index = 1
  hdays = 1
  
  mases <- mapply( FUN = scaled_error_per_cs_unit, my_object = forecast_object, scale_object = scale_object, 
                   MoreArgs = list(metric_choice = "mase", nrwindows = nrwindows,
                                   horizon = horizon, ts_aggr_orders = ts_aggr_orders)
  )
  
  return(mases)
}

mases_per_ts_per_rwindow <- function(forecast_object, scale_object, horizon, nrwindows, ts_aggr_orders){
  
  n_ts = length(ts_aggr_orders)
  forecast_object <- lapply(forecast_object, combine_columns_rmNAs, ts_aggr_orders = ts_aggr_orders)
  horizonDays = 1
  hday_index = 1
  hdays = 1
  
  mases <- mapply( FUN = scaled_error_per_cs_unit_per_rwindow, my_object = forecast_object, scale_object = scale_object, SIMPLIFY = FALSE,
                   MoreArgs = list(metric_choice = "mase", nrwindows = nrwindows,
                                   horizon = horizon, ts_aggr_orders = ts_aggr_orders)
  )
  
  return(mases)
}

scaled_error_per_cs_unit_per_rwindow <- function(my_object, scale_object, metric_choice = "rmsse", nrwindows, horizon, ts_aggr_orders){
  n_ts = length(ts_aggr_orders)
  
  results = matrix(NA, nrwindows, n_ts)
  colnames(results) <- paste0("m",ts_aggr_orders)
  for(ts in 1:n_ts){
    miter <- ts_aggr_orders[ts]
    my_object_ts = my_object[[ts]]
    scale_object_ts = scale_object[[ts]]
    for(i in 1:nrwindows){
      my_object_ts_i = my_object_ts[seq(from = 1+(horizon/miter)*(i-1), length = (horizon/miter), by = 1),, drop = F]
      scale_object_ts_i = scale_object_ts[,i]
      if(metric_choice == "rmsse"){
        results[i,ts] = rmsse(actual = my_object_ts_i[,1], forecast = my_object_ts_i[,2], naive_forecast_error = scale_object_ts_i)
      }
      if(metric_choice == "mase"){
        results[i,ts] = mase(actual = my_object_ts_i[,1], forecast = my_object_ts_i[,2], naive_forecast_error = scale_object_ts_i)
      }
    }
  }
  return(results)
}

scaled_error_per_cs_unit <- function(my_object, scale_object, metric_choice = "rmsse", nrwindows, horizon, ts_aggr_orders){
  n_ts = length(ts_aggr_orders)
  
  results = matrix(NA, nrwindows, n_ts)
  colnames(results) <- paste0("m",ts_aggr_orders)
  for(ts in 1:n_ts){
    miter <- ts_aggr_orders[ts]
    my_object_ts = my_object[[ts]]
    scale_object_ts = scale_object[[ts]]
    for(i in 1:nrwindows){
      my_object_ts_i = my_object_ts[seq(from = 1+(horizon/miter)*(i-1), length = (horizon/miter), by = 1),, drop = F]
      scale_object_ts_i = scale_object_ts[,i]
      if(metric_choice == "rmsse"){
        results[i,ts] = rmsse(actual = my_object_ts_i[,1], forecast = my_object_ts_i[,2], naive_forecast_error = scale_object_ts_i)
      }
      if(metric_choice == "mase"){
        results[i,ts] = mase(actual = my_object_ts_i[,1], forecast = my_object_ts_i[,2], naive_forecast_error = scale_object_ts_i)
      }
    }
  }
  metric = colMeans(results)
  names(metric) <- paste0("m",ts_aggr_orders)
  return(metric)
}

### root mean squared scaled error
rmsse <- function(actual, forecast, naive_forecast_error){
  forecast <- round(forecast)
  num <- mean((actual - forecast)^2)
  denum <- mean((naive_forecast_error)^2)
  metric <- sqrt(num/denum)
  return(metric)
}

### mean absolute scaled error
mase <- function(actual, forecast, naive_forecast_error){
  forecast <- round(forecast)
  num <- mean(abs(actual - forecast))
  denum <- mean(abs(naive_forecast_error))
  metric <- (num/denum)
  return(metric)
}