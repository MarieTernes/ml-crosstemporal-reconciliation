# Main Results WAPE

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

zone_selected = "NYC"
setwd(paste0("./", zone_selected))

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

# parameters
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
  
  if(base_method == "naive"){
    fcst_base <- get(load(paste0(base_method_name, "_base_forecasts_oos.RData")))
    
    fcst_rf <- get(load(paste0(base_method_name, "_randomforest_", ML_predictor,"_recon_forecasts_oos.RData")))
    fcst_xgb_sq <- get(load(paste0(base_method_name, "_xgboost_squared_", ML_predictor,"_recon_forecasts_oos.RData")))
    fcst_lgbm_sq <- get(load(paste0(base_method_name, "_lightgbm_squared_", ML_predictor, "_recon_forecasts_oos.RData")))
    
  }else{
    fcst_base <- get(load(paste0(base_method_name, "_base_forecasts_oos.RData")))
    fcst_bu <- get(load(paste0(base_method_name, "_bottomup_", ML_predictor ,"_recon_forecasts_oos.RData")))
    
    fcst_tcs <- get(load(paste0(base_method_name, "_tcs_t-wlsv_cs-shr_recon_forecasts_oos.RData")))
    fcst_cst <- get(load(paste0(base_method_name, "_cst_t-wlsv_cs-shr_recon_forecasts_oos.RData")))
    fcst_ite_hts <- get(load(paste0(base_method_name, "_ite_t-wlsv_cs-shr_hts_recon_forecasts_oos.RData")))
    fcst_oct <- get(load(paste0(base_method_name, "_oct_wlsv_recon_forecasts_oos.RData")))
    
    fcst_rf <- get(load(paste0(base_method_name, "_randomforest_", ML_predictor,"_recon_forecasts_oos.RData")))
    fcst_xgb_sq <- get(load(paste0(base_method_name, "_xgboost_squared_", ML_predictor,"_recon_forecasts_oos.RData")))
    fcst_lgbm_sq <- get(load(paste0(base_method_name, "_lightgbm_squared_", ML_predictor, "_recon_forecasts_oos.RData")))
  }
  
  
  if(base_method == "naive"){
    wape_base_summary <-  wape_summary_upd(fcst_base, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders , hday_index = hday_index, 
                                           horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_rf_summary <-  wape_summary_upd(fcst_rf, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                         horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_xgb_sq_summary <-  wape_summary_upd(fcst_xgb_sq, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                             horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_lgbm_sq_summary <-  wape_summary_upd(fcst_lgbm_sq, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                              horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wapes_all = cbind(wape_base_summary,
                      wape_rf_summary, 
                      wape_xgb_sq_summary,
                      wape_lgbm_sq_summary
    )
    colnames(wapes_all) <- paste(main_name[bs], 
                                 c("Base", 
                                   "Randomforest",
                                   "XGBoost", 
                                   "LightGBM") 
    )
    
  }else{
    wape_base_summary <-  wape_summary_upd(fcst_base, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                           horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_bu_summary <-  wape_summary_upd(fcst_bu, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                         horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_tcs_summary <-  wape_summary_upd(fcst_tcs, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                          horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_cst_summary <-  wape_summary_upd(fcst_cst, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                          horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_ite_hts_summary <-  wape_summary_upd(fcst_ite_hts, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                              horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_oct_summary <-  wape_summary_upd(fcst_oct, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                          horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_rf_summary <-  wape_summary_upd(fcst_rf, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                         horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_xgb_sq_summary <-  wape_summary_upd(fcst_xgb_sq, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                             horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    wape_lgbm_sq_summary <-  wape_summary_upd(fcst_lgbm_sq, n_cs = n_cs, n_cs_up = n_cs_up, n_cs_bt = n_cs_bt, cs_levels = cs_levels, ts_aggr_orders = ts_aggr_orders, hday_index = hday_index, 
                                              horizonDays = horizonDays, combine_columns_rmNAs_flag = combine_columns_rmNAs_flag)
    
    wapes_all = cbind(wape_base_summary, wape_bu_summary, wape_tcs_summary, wape_cst_summary, wape_ite_hts_summary, wape_oct_summary,
                      wape_rf_summary, 
                      wape_xgb_sq_summary, 
                      wape_lgbm_sq_summary
    )
    colnames(wapes_all) <- paste(main_name[bs], 
                                 c("Base", "Bottom Up", "tcs", "cst","ite_hts", "oct", 
                                   "Randomforest",
                                   "XGBoost",
                                   "LightGBM")
    )
    
  }
  if(bs == 1){
    Table = t(wapes_all)
  }else{
    Table = rbind(Table,t(wapes_all))
  }
}


Table6 = Table[,1:10]
Table7 = Table[,11:20]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
