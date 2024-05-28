# Sensitivity Results features WAPE

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

zone_selected = "NYC"
setwd(paste0("./", zone_selected, "/cstempall"))

### Base method parameters ###
base_method_list = list("naive", "ets", "sarima", "averageBF")
main_name = list("naive","ETS", "SARIMA", "Forecast Combination")
fourier_dummy = FALSE # only for ets and sarima 
optimizeK = TRUE # only for ets and sarima 

ML_predictor = "cstempall"

# parameters for metric
hday_index <- 1
horizonDays <- 1
combine_columns_rmNAs_flag = FALSE

n_cs = 7
n_cs_up = 1
n_cs_bt = 6
cs_levels = 2
ts_aggr_orders = c(1,2,3,4,6,8,12,16,24,48)


for(bs in 1:length(base_method_list)){
  base_method = base_method_list[bs]
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
  if(base_method == "naive" | base_method == "lr" | base_method == "averageBF"){
    base_method_name = base_method
  }
  
  fcst_rf_complete <- get(load(paste0(base_method_name, "_randomforest_", ML_predictor,"_recon_forecasts_oos.RData")))
  fcst_xgb_sq_complete <- get(load(paste0(base_method_name, "_xgboost_squared_", ML_predictor,"_recon_forecasts_oos.RData")))
  fcst_lgbm_sq_complete <- get(load(paste0(base_method_name, "_lightgbm_squared_", ML_predictor, "_recon_forecasts_oos.RData")))
  
  wape_rf_complete_summary <-  wape_summary_upd(fcst_rf_complete, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                       horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  wape_xgb_sq_complete_summary <-  wape_summary_upd(fcst_xgb_sq_complete, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                           horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  wape_lgbm_sq_complete_summary <-  wape_summary_upd(fcst_lgbm_sq_complete, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                            horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  
  wapes_all_complete = cbind(wape_rf_complete_summary, 
                    wape_xgb_sq_complete_summary, 
                    wape_lgbm_sq_complete_summary
  )
  colnames(wapes_all_complete) <- paste(main_name[bs], 
                               c("Randomforest",
                                 "XGBoost",
                                 "LightGBM")
  )
    
  if(bs == 1){
    Table_complete = t(wapes_all_complete)
  }else{
    Table_complete = rbind(Table_complete,t(wapes_all_complete))
  }
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(paste0(".././main/", zone_selected))

### Base method parameters ###
base_method_list = list("naive", "ets", "sarima", "averageBF")
main_name = list("Naive","ETS", "SARIMA", "Forecast Combination")
fourier_dummy = FALSE # only for ets and sarima 
optimizeK = TRUE # only for ets and sarima 

ML_predictor = "cstemp"

# parameters for metric
hday_index <- 1
horizonDays <- 1
combine_columns_rmNAs_flag = FALSE

n_cs = 7
n_cs_up = 1
n_cs_bt = 6
cs_levels = 2
ts_aggr_orders = c(1,2,3,4,6,8,12,16,24,48)


for(bs in 1:length(base_method_list)){
  base_method = base_method_list[bs]
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
  if(base_method == "naive" | base_method == "lr" | base_method == "averageBF"){
    base_method_name = base_method
  }
  
  fcst_rf_compact <- get(load(paste0(base_method_name, "_randomforest_", ML_predictor,"_recon_forecasts_oos.RData")))
  fcst_xgb_sq_compact <- get(load(paste0(base_method_name, "_xgboost_squared_", ML_predictor,"_recon_forecasts_oos.RData")))
  fcst_lgbm_sq_compact <- get(load(paste0(base_method_name, "_lightgbm_squared_", ML_predictor, "_recon_forecasts_oos.RData")))
  
  wape_rf_compact_summary <-  wape_summary_upd(fcst_rf_compact, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                       horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  wape_xgb_sq_compact_summary <-  wape_summary_upd(fcst_xgb_sq_compact, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                           horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  wape_lgbm_sq_compact_summary <-  wape_summary_upd(fcst_lgbm_sq_compact, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                            horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
  
  wapes_all_compact = cbind(wape_rf_compact_summary, 
                    wape_xgb_sq_compact_summary, 
                    wape_lgbm_sq_compact_summary
  )
  colnames(wapes_all_compact) <- paste(main_name[bs], 
                               c("Randomforest",
                                 "XGBoost",
                                 "LightGBM")
  )
    
  if(bs == 1){
    Table_compact = t(wapes_all_compact)
  }else{
    Table_compact = rbind(Table_compact,t(wapes_all_compact))
  }
}

relaTable = Table_complete/Table_compact
TableB1_H3cells = relaTable[,1:10]
TableB1_market = relaTable[,11:20]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
