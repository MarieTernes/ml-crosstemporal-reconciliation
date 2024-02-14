# Functions
zone_data_prep_fct <- function(data, data_start_index, catchment_areas_zone, ncols_info, catchment_area_level = TRUE){
  catchment_areas_zone_sorted = catchment_areas_zone[order(catchment_areas_zone$catchment_area, catchment_areas_zone$delivery_area),]
  sorted_indices = match(catchment_areas_zone_sorted$delivery_area, colnames(data))
  sorted_indices_rmNAs = sorted_indices[which(!is.na(sorted_indices))] # take out NAs
  catchment_areas_zone_updated = catchment_areas_zone_sorted[which(!is.na(sorted_indices)),]
  delivery_areas = catchment_areas_zone_updated$delivery_area
  nbr_areas = length(delivery_areas)
  catchment_areas = unique(catchment_areas_zone_updated$catchment_area)
  
  data_cols = c(1:ncols_info, sorted_indices_rmNAs)
  zone_data_info = data[data_start_index:nrow(data), data_cols]
  zone_data = data[data_start_index:nrow(data), sorted_indices_rmNAs]
  
  if(catchment_area_level){
    CCA = t(dummy_cols(catchment_areas_zone_updated, select_columns = "catchment_area")[,-c(1:2)])
    rownames(CCA) = catchment_areas
    colnames(CCA) = delivery_areas
    
    CT = rep(1, nbr_areas)
    C = rbind(CT, CCA)
    rownames(C)[1] = "Total"
    
    n_cs_level <- c(1,length(catchment_areas), nbr_areas)
  }else{
    C = matrix(1, nrow = 1, ncol = nbr_areas)
    rownames(C) = "Total"
    colnames(C) = delivery_areas
    n_cs_level <- c(1, nbr_areas)
  }
  
  bottom = as.matrix(zone_data)
  upper = bottom %*% t(C)
  zone_data_final = cbind(upper, bottom)
  zone_data_final_info = data.frame(data[data_start_index:nrow(data),1:ncols_info], zone_data_final)
  
  list_out = list(area_data_bt_hf = zone_data_info, area_data_cs_hf = zone_data_final_info, C = C, catchment_areas_mat = catchment_areas_zone_updated, 
                  catchment_areas = catchment_areas, delivery_areas = delivery_areas, n_cs = sum(n_cs_level), n_cs_bt = nbr_areas, n_cs_level = n_cs_level)
  return(list_out)
}



fit_rf <- function(y, X, Xout, parlist = NULL){
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  
  # START CODE
  train <- cbind(y, X) %>%
    as.data.frame() %>%
    clean_names()
  
  test <-  as.data.frame(Xout) %>%
    clean_names()
  
  X_train <- subset(train,select = -y)
  
  if(is.null(parlist)){
    mtry=if (!is.null(y) && !is.factor(y))
      max(floor(ncol(X)/3), 1) else floor(sqrt(ncol(X)))
    nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1
    ntree = 500
  }else{
    mtry = parlist$mtry
    nodesize = parlist$nodesize
    ntree = parlist$ntree
  }
  
  rf.model <- randomForest(y ~., 
                           data = train, 
                           mtry = mtry, nodesize = nodesize, ntree = ntree,
                           importance = FALSE) # We use default values of the tuning parameters
  # Defaults randomForest
  # ntree : scalar, number of trees. Default value: 500
  # mtry: scalar, number of variables tried at each split. Default value = total number of variables / 3
  
  # OUTPUT
  # forecasts : vector containing h-step ahead demand forecasts
  forecasts = pmax(as.vector(predict(rf.model, test)), 0) # Avoid negative forecasts
  
  
  # Output
  out <- list("forecasts" = forecasts, "fit" = rf.model, "Xin" = X_train, "Xout" = test)
}



fit_xgboost <- function(y, X, Xout, parlist = NULL){
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  # START CODE
  train <- cbind(y, X) %>% as.data.frame() %>% clean_names()
  
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() 
  train.y <- train.y[,1]
  
  test.x <- Xout %>% as.data.frame() %>% clean_names() %>% as.matrix()
  
  if(is.null(parlist)){
    params <- list(
      eta = 0.3,
      colsample_bytree = 1,
      min_child_weight = 1,
      max_depth = 6,
      gamma = 0,
      subsample = 1,
      objective = "reg:squarederror"
    )
    nrounds = 100
  }else{
    params = list(
      eta = parlist$eta,
      colsample_bytree = parlist$colsample_bytree,
      min_child_weight = parlist$min_child_weight,
      max_depth = parlist$max_depth,
      gamma = parlist$gamma,
      subsample = parlist$subsample,
      objective = "reg:squarederror"
    )
    nrounds = parlist$nrounds 
  }

  DMtrain <- xgb.DMatrix(data = train.x, label = train.y)
  model.xgb <- xgb.train(data = DMtrain, nrounds = nrounds, params = params, verbose = 0)
  
  # OUTPUT
  fitted_values <- pmax(as.vector(predict(model.xgb, train.x)), 0)
  forecasts <- pmax(as.vector(predict(model.xgb, test.x)), 0)
  
  out <- list("forecasts" = forecasts, "fit" = model.xgb, "fitted_values" = fitted_values, "y" = train.y)
}

fit_xgboost_tweedie <- function(y, X, Xout, parlist = NULL, tweedie_variance_power = 1.5){
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  # START CODE
  train <- cbind(y, X) %>% as.data.frame() %>% clean_names()
  
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names()
  train.y <- train.y[,1]
  
  test.x <- Xout %>% as.data.frame() %>% clean_names() %>% as.matrix()
  
  if(is.null(parlist)){
    params <- list(
      eta = 0.3,
      colsample_bytree = 1,
      min_child_weight = 1,
      max_depth = 6,
      gamma = 0,
      subsample = 1,
      objective = "reg:tweedie",
      tweedie_variance_power = tweedie_variance_power
    )
    nrounds = 100
  }else{
    params = list(
      eta = parlist$eta,
      colsample_bytree = parlist$colsample_bytree,
      min_child_weight = parlist$min_child_weight,
      max_depth = parlist$max_depth,
      subsample = parlist$subsample,
      objective = "reg:tweedie",
      tweedie_variance_power = parlist$tweedie_variance_power
    )
    nrounds = parlist$nrounds  
  }

  DMtrain <- xgb.DMatrix(data = train.x, label = train.y)
  model.xgb <- xgb.train(data = DMtrain, nrounds = nrounds, params = params, verbose = 0)
  
  # OUTPUT
  fitted_values <- pmax(as.vector(predict(model.xgb, train.x)), 0)
  forecasts <- pmax(as.vector(predict(model.xgb, test.x)), 0)

  out <- list("forecasts" = forecasts, "fit" = model.xgb, "fitted_values" = fitted_values, "y" = train.y)
}

fit_lightgbm <- function(y, X, Xout, parlist = NULL){
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  # START CODE
  train <- cbind(y, X) %>% as.data.frame() %>% clean_names()
  
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() #%>% as.vector()
  train.y <- train.y[,1]
  
  test.x <- Xout %>% as.data.frame() %>% clean_names() %>% as.matrix()
  
  if(is.null(parlist)){
    params <- list(
      eta = 0.1,
      num_leaves = 31,
      subsample = 1,
      colsample_bytree = 1,
      min_child_weight = 1e-3,
      max_depth = -1,
      lambda_l1 = 0,
      objective = "regression"
    )
    nrounds = 100
  }else{
    params = list(
      eta = parlist$eta,
      num_leaves = parlist$num_leaves,
      subsample = parlist$subsample, 
      colsample_bytree = parlist$colsample_bytree,
      min_child_weight = parlist$min_child_weight,
      max_depth = parlist$max_depth,
      lambda_l1 = parlist$lambda_l1,
      objective = "regression"
    )
    nrounds = parlist$nrounds  
  }
  
  dtrain <- lgb.Dataset(data = train.x, label = train.y)
  model.lgb <- lgb.train(data = dtrain, params = params, nrounds = nrounds, verbose = -1)
  
  # OUTPUT
  fitted_values <- pmax(as.vector(predict(model.lgb, train.x)), 0)
  forecasts <- pmax(as.vector(predict(model.lgb, test.x)), 0)
  
  out <- list("forecasts" = forecasts, "fit" = model.lgb, "fitted_values" = fitted_values, "y" = train.y)
}

fit_lightgbm_tweedie <- function(y, X, Xout, parlist = NULL, tweedie_variance_power = 1.5){
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  # START CODE
  train <- cbind(y, X) %>% as.data.frame() %>% clean_names()
  
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() #%>% as.vector()
  train.y <- train.y[,1]
  
  test.x <- Xout %>% as.data.frame() %>% clean_names() %>% as.matrix()
  
  if(is.null(parlist)){
    params <- list(
      eta = 0.1,
      num_leaves = 31,
      subsample = 1,
      colsample_bytree = 1,
      min_child_weight = 1e-3,
      max_depth = -1,
      lambda_l1 = 0,
      objective = "tweedie",
      tweedie_variance_power = tweedie_variance_power
    )
    nrounds = 100
  }else{
    params = list(
      eta = parlist$eta,
      num_leaves = parlist$num_leaves,
      subsample = parlist$subsample, 
      colsample_bytree = parlist$colsample_bytree,
      min_child_weight = parlist$min_child_weight,
      max_depth = parlist$max_depth,
      lambda_l1 = parlist$lambda_l1,
      objective = "tweedie",
      tweedie_variance_power = parlist$tweedie_variance_power
    )
    nrounds = parlist$nrounds 
  }
  
  dtrain <- lgb.Dataset(data = train.x, label = train.y)
  model.lgb <- lgb.train(data = dtrain, params = params, nrounds = nrounds, verbose = -1)
  
  # OUTPUT
  fitted_values <- pmax(as.vector(predict(model.lgb, train.x)), 0)
  forecasts <- pmax(as.vector(predict(model.lgb, test.x)), 0)
  
  out <- list("forecasts" = forecasts, "fit" = model.lgb, "fitted_values" = fitted_values, "y" = train.y)
}



aggregate_ts_hierachy <- function(X, agg_orders){
  X_agg_list <- vector(mode = "list", length = length(agg_orders))
  names(X_agg_list) <- paste0("m",agg_orders) #c("min30", "hourly","daily","weekly") 
  X_agg_list[[1]] = X
  
  for(tl in 2:(length(agg_orders))){
    X_agg = apply(X, 2, agg_ts, agg_order =  agg_orders[tl])
    if(class(X_agg)[1] == "numeric"){
      X_agg <- t(as.matrix(X_agg))
    }
    X_agg_list[[tl]] = X_agg
  }
  
  return(X_agg_list)
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


#### SMAPE FUNCTION ####
SMAPE_between_0_100 <- function(forecast, actual){ 
  forecast <- round(forecast)
  # https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
  # This produces a value between 0-100. You just need to remove the division by 2 in the denominator
  num <- abs(forecast  - actual)
  denom <- (abs(actual) + abs(forecast))
  ratio <- num/denom
  ratio[which(ratio==Inf)] = NA 
  metric <- mean(num/denom, na.rm = TRUE)*100
  return(metric)
}

# constructs data matrix consisting of fourier_series
fourier_series_own <- function(seas_freq, n, K = 1){
  s <-  rep(1:seas_freq, n)
  sp <- 2*pi*s/seas_freq
  X = matrix(NA, nrow = length(s), ncol = K*2)
  
  for(k in 1:K){
    x = cbind(cos(k*sp), sin(k*sp))
    colnames(x) = c(paste0("cos_", k, seas_freq), paste0("sin_", k, seas_freq))
    X[,(k*2-1):(2*k)] = x
  }
  
  return(X)
}


fit_ets_own <- function(y, seas_freq, K = floor(seas_freq/2), m, horizon, fourier_dummy = TRUE, optimizeK = TRUE){
  
  if(fourier_dummy){
    if(seas_freq > 1){
      if (2 * K > seas_freq) {
        stop("K must be not be greater than seas_freq/2")
      }
      if(optimizeK){
        bic = rep(NA, K)
        for(k in 1:K){
          Xfourier_in = fourier(ts(y, frequency = seas_freq), K = k)
          fit_fourier_k <- lm(y ~ Xfourier_in)
          bic[k] = BIC(fit_fourier_k)
        }
        kstar = which.min(bic)
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = kstar)
        Xfourier_out = fourier(ts(y, frequency = seas_freq), K = kstar, h = horizon/m)
      }else{
        kstar = K
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = K)
        Xfourier_out = fourier(ts(y, frequency = seas_freq), K = K, h = horizon/m)
      }
      fit_fourier <-  lm(y ~ Xfourier_in)
      fit_ets <- ets(ts(fit_fourier$residuals), ic = "aicc")
      fourier_forecasts <- forecast(fit_fourier, newdata = data.frame(Xfourier_out), h = horizon/m)
      fit_insample <- pmax(0, fit_fourier$fitted.values + fit_ets$fitted)
      ets_resid <- y - fit_insample
      resid_insample <- ets_resid
      
      ets_forecasts <- forecast(fit_ets, h = horizon/m)
      ets_forecasts_oos <- fourier_forecasts$mean + ets_forecasts$mean
      ets_forecasts_oos <- pmax(0, ets_forecasts_oos)
      fcst_oos_ts <- ets_forecasts_oos
    }else{
      kstar = NA
      fit_ets <- ets(ts(y), ic = "aicc")
      fit_insample <- pmax(0, fit_ets$fitted)
      ets_resid <- y - fit_insample
      resid_insample <- ets_resid
      ets_forecasts <- forecast(fit_ets, h = horizon/m)
      ets_forecasts_oos <- pmax(0, ets_forecasts$mean)
      fcst_oos_ts <- ets_forecasts_oos
    }
  }else{
    if(seas_freq >= 17){
      bic = rep(NA, K)
      for(k in 1:K){
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = k)
        fit_fourier_k <- lm(y ~ Xfourier_in)
        bic[k] = BIC(fit_fourier_k)
      }
      kstar = which.min(bic)
      Xfourier_in = fourier(ts(y, frequency = seas_freq), K = kstar)
      Xfourier_out = fourier(ts(y, frequency = seas_freq), K = kstar, h = horizon/m)
      fit_fourier <-  lm(y ~ Xfourier_in)
      fit_ets <- ets(ts(fit_fourier$residuals), ic = "aicc")
      fourier_forecasts <- forecast(fit_fourier, newdata = data.frame(Xfourier_out), h = horizon/m)
      fit_insample <- pmax(0, fit_fourier$fitted.values + fit_ets$fitted)
      ets_resid <- y - fit_insample
      resid_insample <- ets_resid
      
      ets_forecasts <- forecast(fit_ets, h = horizon/m)
      ets_forecasts_oos <- fourier_forecasts$mean + ets_forecasts$mean
      ets_forecasts_oos <- pmax(0, ets_forecasts_oos)
      fcst_oos_ts <- ets_forecasts_oos
    }else{
      kstar = NA
      fit_ets <- ets(ts(y, frequency = seas_freq), ic = "aicc")
      fit_insample <- pmax(0, fit_ets$fitted)
      ets_resid <- y - fit_insample
      resid_insample <- ets_resid
      ets_forecasts <- forecast(fit_ets, h = horizon/m)
      ets_forecasts_oos <- pmax(0, ets_forecasts$mean)
      fcst_oos_ts <- ets_forecasts_oos
    }
  }
  return(list("forecast" = fcst_oos_ts, "residuals" = resid_insample, "Kstar" = kstar, "fit" = fit_ets))
}


fit_sarima_own <- function(y, seas_freq, K = floor(seas_freq/2), m, horizon, fourier_dummy = TRUE, optimizeK = TRUE){
  
  if(fourier_dummy){
    if(seas_freq > 1){
      if (2 * K > seas_freq) {
        stop("K must be not be greater than seas_freq/2")
      }
      if(optimizeK){
        bic = rep(NA, K)
        for(k in 1:K){
          Xfourier_in = fourier(ts(y, frequency = seas_freq), K = k)
          fit_fourier_k <- lm(y ~ Xfourier_in)
          bic[k] = BIC(fit_fourier_k)
        }
        kstar = which.min(bic)
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = kstar)
        Xfourier_out = fourier(ts(y, frequency = seas_freq), K = kstar, h = horizon/m)
      }else{
        kstar = K
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = K)
        Xfourier_out = fourier(ts(y, frequency = seas_freq), K = K, h = horizon/m)
      }
      fit_fourier <-  lm(y ~ Xfourier_in)
      fit_arima <- auto.arima(ts(fit_fourier$residuals), seasonal = FALSE, ic = "aicc", approximation = T, max.d = 1, max.D = 0) 
      fit_insample <- pmax(0, fit_fourier$fitted.values + fit_arima$fitted)
      arima_resid <- y - fit_insample
      resid_insample <- arima_resid
      fourier_forecasts <- forecast(fit_fourier, newdata = data.frame(Xfourier_out), h = horizon/m)
      arima_forecasts <- forecast(fit_arima, h = horizon/m)
      arima_forecasts_oos <- fourier_forecasts$mean + arima_forecasts$mean
      arima_forecasts_oos <- pmax(0, arima_forecasts_oos)
      fcst_oos_ts <- arima_forecasts_oos
    }else{
      kstar = NA
      fit_arima <- auto.arima(ts(y), seasonal = FALSE, ic = "aicc", approximation = T, max.d = 1, max.D = 0)
      fit_insample <- pmax(0, fit_arima$fitted)
      arima_resid <- y - fit_insample
      resid_insample <- arima_resid
      arima_forecasts <- forecast(fit_arima, h = horizon/miter)
      arima_forecasts_oos <- pmax(0, arima_forecasts$mean)
      fcst_oos_ts <- arima_forecasts_oos
    }
  }else{
    if(seas_freq >= 17){
      bic = rep(NA, K)
      for(k in 1:K){
        Xfourier_in = fourier(ts(y, frequency = seas_freq), K = k)
        fit_fourier_k <- lm(y ~ Xfourier_in)
        bic[k] = BIC(fit_fourier_k)
      }
      kstar = which.min(bic)
      Xfourier_in = fourier(ts(y, frequency = seas_freq), K = kstar)
      Xfourier_out = fourier(ts(y, frequency = seas_freq), K = kstar, h = horizon/m)
      fit_fourier <-  lm(y ~ Xfourier_in)
      fit_arima <- auto.arima(ts(fit_fourier$residuals), seasonal = FALSE, ic = "aicc", approximation = T, max.d = 1, max.D = 0) 
      fit_insample <- pmax(0, fit_fourier$fitted.values + fit_arima$fitted)
      arima_resid <- y - fit_insample
      resid_insample <- arima_resid
      fourier_forecasts <- forecast(fit_fourier, newdata = data.frame(Xfourier_out), h = horizon/m)
      arima_forecasts <- forecast(fit_arima, h = horizon/m)
      arima_forecasts_oos <- fourier_forecasts$mean + arima_forecasts$mean
      arima_forecasts_oos <- pmax(0, arima_forecasts_oos)
      fcst_oos_ts <- arima_forecasts_oos
    }else{
      kstar = NA
      fit_arima <- auto.arima(ts(y, frequency = seas_freq), seasonal = TRUE, ic = "aicc", approximation = T, max.p = 3, max.q = 3, max.d=1, max.P=1,max.Q=1, max.D=1)
      fit_insample <- pmax(0, fit_arima$fitted)
      arima_resid <- y - fit_insample
      resid_insample <- arima_resid
      arima_forecasts <- forecast(fit_arima, h = horizon/m)
      arima_forecasts_oos <- pmax(0, arima_forecasts$mean)
      fcst_oos_ts <- arima_forecasts_oos
    }
  }
  return(list("forecast" = fcst_oos_ts, "residuals" = resid_insample, "Kstar" = kstar, "fit" = fit_arima))
}



dummy_function_package <- function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer, 
                                    verbose = FALSE){
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1) 
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if (drop == FALSE && class(x) == "factor") {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose) 
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x), 
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  mm <- model.matrix(~x - 1, model.frame(~x - 1), contrasts = FALSE)
  colnames.mm <- colnames(mm)
  if (verbose) 
    cat(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL, 
                                                                          colnames.mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), 
                      colnames(mm))
  if (!is.null(row.names(data))) 
    rownames(mm) <- rownames(data)
  return(mm)
}



parameter_tuning_xgb <- function(y, X, Dhour, days, left_out_days = 7, seed = 1){
  set.seed(seed)
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() 
  train.y <- train.y[,1]
  
  dtrain <- xgb.DMatrix(data = train.x, label = train.y)
  
  dtrain = dtrain
  cv_folds <- vector('list', days/left_out_days)
  for(i in 1:(days/left_out_days)){
    cv_folds[[i]] =  (1 + (i-1)*Dhour*left_out_days):((i)*Dhour*left_out_days)
  }
  
  xgb_cv_bayes <- function(eta, subsample, colsample_bytree, min_child_weight, max_depth,
                           gamma, nrounds) {
    cv <- xgb.cv(params = list(eta = eta, subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               min_child_weight = min_child_weight,
                               max_depth = max_depth,
                               gamma = gamma,
                               objective = "reg:squarederror",
                               eval_metric = "rmse"),
                 data = dtrain, nround = nrounds,
                 folds = cv_folds, prediction = TRUE, showsd = TRUE,
                 early_stopping_rounds = 5,maximize = FALSE, verbose = 1)
    list(Score = -1*(cv$evaluation_log$test_rmse_mean[cv$best_iteration]),
         Pred = cv$pred)
  }
  
  bounds = list(eta = c(0.01, 0.05),
                subsample = c(0.3, 1),
                colsample_bytree = c(0.3, 1),
                min_child_weight = c(0L, 10L),
                max_depth = c(2L,10L),
                gamma = c(0,5), 
                nrounds = c(50L,200L)
  )
  
  init_points = length(bounds)*5
  n_iter = 10
  
  OPT_Res <- try(BayesianOptimization(xgb_cv_bayes,
                                      bounds = bounds,
                                      init_grid_dt = NULL, init_points = init_points, n_iter = n_iter,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = FALSE)) 
  
  return(list("OPT_Res" = OPT_Res, "bounds" = bounds))
}

parameter_tuning_xgb_tweedie <- function(y, X, Dhour, days, left_out_days = 7, seed = 1){
  set.seed(seed)
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() 
  train.y <- train.y[,1]
  
  dtrain <- xgb.DMatrix(data = train.x, label = train.y)
  
  dtrain = dtrain
  cv_folds <- vector('list', days/left_out_days)
  for(i in 1:(days/left_out_days)){
    cv_folds[[i]] =  (1 + (i-1)*Dhour*left_out_days):((i)*Dhour*left_out_days)
  }
  
  xgb_cv_bayes <- function(eta, subsample, colsample_bytree, min_child_weight, max_depth,
                           gamma, tweedie_variance_power, nrounds) {
    cv <- xgb.cv(params = list(eta = eta, subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               min_child_weight = min_child_weight,
                               max_depth = max_depth,
                               gamma = gamma,
                               tweedie_variance_power = tweedie_variance_power,
                               objective = "reg:tweedie",
                               eval_metric = "rmse"),
                 data = dtrain, nround = nrounds,
                 folds = cv_folds, prediction = TRUE, showsd = TRUE,
                 early_stopping_rounds = 5,maximize = FALSE, verbose = 1)
    list(Score = -1*(cv$evaluation_log$test_rmse_mean[cv$best_iteration]),
         Pred = cv$pred)
  }
  
  bounds = list(eta = c(0.01, 0.05),
                subsample = c(0.3, 1),
                colsample_bytree = c(0.3, 1),
                min_child_weight = c(0L, 10L),
                max_depth = c(2L,10L),
                gamma = c(0,5), 
                tweedie_variance_power = c(1.1, 1.9),
                nrounds = c(50L,200L)
  )
  
  init_points = length(bounds)*5
  n_iter = 10
  
  OPT_Res <- try(BayesianOptimization(xgb_cv_bayes,
                                      bounds = bounds,
                                      init_grid_dt = NULL, init_points = init_points, n_iter = n_iter,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = FALSE)) 
  
  return(list("OPT_Res" = OPT_Res, "bounds" = bounds))
}


parameter_tuning_lgbm <- function(y, X, Dhour, days, left_out_days = 7, seed = 1){
  set.seed(seed)
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names() 
  train.y <- train.y[,1]
  
  dtrain <- lgb.Dataset(data = train.x, label = train.y)
  
  cv_folds <- vector('list', days/left_out_days)
  for(i in 1:(days/left_out_days)){
    cv_folds[[i]] =  (1 + (i-1)*Dhour*left_out_days):((i)*Dhour*left_out_days)
  }
  
  lgbm_cv_bayes <- function(eta, num_leaves, subsample, colsample_bytree, min_child_weight, 
                            max_depth, lambda_l1,
                            nrounds) {
    cv <- lgb.cv(params = list(eta = eta, 
                               num_leaves = num_leaves,
                               subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               min_child_weight = min_child_weight,
                               max_depth = max_depth,
                               lambda_l1 = lambda_l1,
                               objective = "regression",
                               metric = "rmse"),
                 data = dtrain, nround = nrounds,
                 folds = cv_folds, showsd = TRUE,
                 early_stopping_rounds = 5, verbose = 0)
    list(Score = -1*(unlist(cv$record_evals$valid$rmse$eval[cv$best_iter])),
         Pred = 0) 
  }
  
  bounds = list(eta = c(0.01, 0.05),
                num_leaves = c(5L, 31L),
                subsample = c(0.3, 1),
                colsample_bytree = c(0.3, 1),
                min_child_weight = c(0L, 10L),
                max_depth = c(2L,10L),
                lambda_l1 = c(0,5),
                nrounds = c(50L,100L)
  )
  
  init_points = length(bounds)*5
  n_iter = 10
  
  OPT_Res <- try(BayesianOptimization(lgbm_cv_bayes,
                                      bounds = bounds,
                                      init_grid_dt = NULL, init_points = init_points, n_iter = n_iter,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = FALSE)) 
  
  return(list("OPT_Res" = OPT_Res, "bounds" = bounds))
}


parameter_tuning_lgbm_tweedie <- function(y, X, Dhour, days, left_out_days = 7, seed = 1){
  set.seed(seed)
  train.x <- X %>% as.data.frame() %>% clean_names() %>% as.matrix()
  train.y <- y %>% as.data.frame() %>% clean_names()
  train.y <- train.y[,1]
  
  dtrain <- lgb.Dataset(data = train.x, label = train.y)
  
  cv_folds <- vector('list', days/left_out_days)
  for(i in 1:(days/left_out_days)){
    cv_folds[[i]] =  (1 + (i-1)*Dhour*left_out_days):((i)*Dhour*left_out_days)
  }
  
  lgbm_cv_bayes <- function(eta, num_leaves, subsample, colsample_bytree, min_child_weight, 
                            max_depth, lambda_l1,
                            tweedie_variance_power,
                            nrounds) {
    cv <- lgb.cv(params = list(eta = eta, 
                               num_leaves = num_leaves,
                               subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               min_child_weight = min_child_weight,
                               max_depth = max_depth,
                               lambda_l1 = lambda_l1,
                               tweedie_variance_power = tweedie_variance_power,
                               objective = "tweedie",
                               metric = "rmse"),
                 data = dtrain, nround = nrounds,
                 folds = cv_folds, showsd = TRUE,
                 early_stopping_rounds = 5, verbose = 0)
    list(Score = -1*(unlist(cv$record_evals$valid$rmse$eval[cv$best_iter])),
         Pred = 0) 
  }
  
  bounds = list(eta = c(0.01, 0.05),
                num_leaves = c(5L, 31L),
                subsample = c(0.3, 1),
                colsample_bytree = c(0.3, 1),
                min_child_weight = c(0L, 10L),
                max_depth = c(2L,10L),
                lambda_l1 = c(0,5),
                tweedie_variance_power = c(1.1,1.9),
                nrounds = c(50L,100L)
  )
  
  init_points = length(bounds)*5
  n_iter = 10
  
  OPT_Res <- try(BayesianOptimization(lgbm_cv_bayes,
                                      bounds = bounds,
                                      init_grid_dt = NULL, init_points = init_points, n_iter = n_iter,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = FALSE)) 
  
  return(list("OPT_Res" = OPT_Res, "bounds" = bounds))
}


parameter_tuning_rf <- function(y, X, Dhour, days, left_out_days = 7, seed = 1){
  set.seed(seed)
  
  # INPUT
  # y : response vector training sample
  # X : predictor matrix training sample
  # Xout : predictor matrix test sample
  
  
  # START CODE
  train <- cbind(y, X) %>%
    as.data.frame() %>%
    clean_names()
  
  
  X_train <- subset(train,select = -y)
  
  # Define the task for regression
  task <- makeRegrTask(data = train, target = "y", blocking = factor(rep(1:(days/left_out_days), each = Dhour*left_out_days)))
  
  # Define the learner for random forest regression
  rf_learner <- makeLearner("regr.randomForest", predict.type = "response")
  
  # Define the parameter space
  param_space <- makeParamSet(
    makeIntegerParam("mtry", lower = 2, upper = 10), #change to 7 for example
    makeIntegerParam("nodesize", lower = 5, upper = 50),
    makeDiscreteParam("ntree", values = seq(50, 500, by = 10))
  )
  
  cv_folds <- vector('list', days/left_out_days)
  for(i in 1:(days/left_out_days)){
    cv_folds[[i]] =  (1 + (i-1)*Dhour*left_out_days):((i)*Dhour*left_out_days)
  }
  
  # Specify custom folds for cross-validation
  rdesc = makeResampleDesc("CV", iters = (days/left_out_days), blocking.cv = TRUE) 
  rin = makeResampleInstance(rdesc, task = task)
  # forcing it to be the same as with cv_folds in xgboost and lightgbm
  for(i in 1:(days/left_out_days)){
    rin$train.inds[[i]] = c(1:((days/left_out_days)*Dhour*left_out_days))[-cv_folds[[i]]] 
    rin$test.inds[[i]] = cv_folds[[i]]  
  }
  
  # Define the tuning control
  ctrl <- makeTuneControlRandom(maxit = 100)
  
  # Perform tuning with custom folds
  tune_result <- tuneParams(learner = rf_learner, task = task, resampling = rin, measures = list(rmse), 
                            par.set = param_space, control = ctrl, show.info = F)
  OPT_Res = tune_result$x
  
  return(list("OPT_Res" = OPT_Res, "bounds" = param_space, "tune_result" = tune_result))
}


#############################################################################################################################
#########------------------------------------- SUMMARIZING FUNCTIONS ----------------------------------------------##########
#############################################################################################################################

fit_base_forecast_validation <- function(x, base_method = "naive", ts_aggr_orders, seasonal_frequency = NULL,
                                         window_hours, Whour, validation, horizonVal, val_indices_fix, rolling_validation, 
                                         n_cs, names_cs = NULL, 
                                         fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = NULL, 
                                         iwindow_first = TRUE, forecasts_val_old = NULL){
  n_ts = length(ts_aggr_orders)
  demand_window_train = x
  ival_last = rolling_validation[length(rolling_validation)] # fix to the last one
  
  if(base_method == "naive"){
    #### Naive Last Week ####
    forecasts_val <- vector(mode = "list", length = n_ts)
    names(forecasts_val) <- paste0("m",ts_aggr_orders)
    for(ts in 1:n_ts){
      miter <- ts_aggr_orders[ts]
      demand_window_ts_train = demand_window_train[[ts]]
      fcst_val = demand_window_ts_train[val_indices_fix[[ts]] - (Whour/miter), , drop = F] 
      forecasts_val[[ts]] <- fcst_val
    }
  }
  
  if(base_method == "ets"){
    #### ETS ####
    forecasts_val <- vector(mode = "list", length = n_ts)
    names(forecasts_val) <- paste0("m",ts_aggr_orders)
    if(iwindow_first){
      for(ts in 1:n_ts){
        ptm_ets_val <- proc.time()
        miter = ts_aggr_orders[ts]
        fcst_val_ts =  matrix(NA, nrow =  validation/miter, ncol = n_cs)
        colnames(fcst_val_ts) <- names_cs
        for(cs in 1:n_cs){
          demand_window_ts_train <- demand_window_train[[ts]][,cs]
          fcst_ival_ts <- c()
          for(ival in rolling_validation){
            demand_ival = demand_window_ts_train[(ival/miter - window_hours/miter + 1) : (ival/miter)] # for remember for later!!
            ets_out <-  fit_ets_own(y = demand_ival, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                    horizon = horizonVal, fourier_dummy = fourier_dummy,
                                    optimizeK = optimizeK)
            fcst_ival_ts = c(fcst_ival_ts, ets_out$forecast)
          }
          fcst_val_ts[,cs] = fcst_ival_ts
        }
        forecasts_val[[ts]] <- fcst_val_ts
        timing_ets <- proc.time() - ptm_ets_val
        cat("validation fit ets for frequency",miter, "takes", timing_ets[3], "seconds", "\n")
      }
    }else{
      for(ts in 1:n_ts){
        ptm_ets_val <- proc.time()
        miter = ts_aggr_orders[ts]
        fcst_ival_ts =  matrix(NA, nrow =  horizonVal/miter, ncol = n_cs)
        colnames(fcst_ival_ts) <- names_cs
        for(cs in 1:n_cs){
          demand_window_ts_train <- demand_window_train[[ts]][,cs]
          demand_ival = demand_window_ts_train[(ival_last/miter - window_hours/miter + 1) : (ival_last/miter)]
          ets_out <-  fit_ets_own(y = demand_ival, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                  horizon = horizonVal, fourier_dummy = fourier_dummy,
                                  optimizeK = optimizeK)
          fcst_ival_ts[,cs] = ets_out$forecast
        }
        fcst_val_ts <- rbind(forecasts_val_old[[ts]][(horizonVal/miter+1):(validation/miter), ], fcst_ival_ts)
        forecasts_val[[ts]] <- fcst_val_ts
        timing_ets <- proc.time() - ptm_ets_val
        cat("validation fit ets for frequency",miter, "takes", timing_ets[3], "seconds", "\n")
      }
    }
  }
  
  if(base_method == "sarima"){
    #### SARIMA ####
    forecasts_val <- vector(mode = "list", length = n_ts)
    names(forecasts_val) <- paste0("m",ts_aggr_orders)
    if(iwindow_first){
      for(ts in 1:n_ts){
        ptm_arima_val <- proc.time()
        miter = ts_aggr_orders[ts]
        fcst_val_ts =  matrix(NA, nrow = validation/miter, ncol = n_cs)
        colnames(fcst_val_ts) <- names_cs
        for(cs in 1:n_cs){
          demand_window_ts_train <- demand_window_train[[ts]][,cs]
          fcst_ival_ts <- c()
          for(ival in rolling_validation){
            demand_ival = demand_window_ts_train[(ival/miter - window_hours/miter + 1) : (ival/miter)] # for remember for later!!
            sarima_out <-  fit_sarima_own(y = demand_ival, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                          horizon = horizonVal, fourier_dummy = fourier_dummy,
                                          optimizeK = optimizeK)
            fcst_ival_ts = c(fcst_ival_ts, sarima_out$forecast)
          }
          fcst_val_ts[,cs] = fcst_ival_ts
        }
        forecasts_val[[ts]] <- fcst_val_ts
        timing_arima <- proc.time() - ptm_arima_val
        cat("validation fit sarima at frequency",miter, "takes", timing_arima[3], "seconds", "\n")
      }
    }else{
      for(ts in 1:n_ts){
        ptm_arima_val <- proc.time()
        miter = ts_aggr_orders[ts]
        fcst_ival_ts =  matrix(NA, nrow = horizonVal/miter, ncol = n_cs)
        colnames(fcst_val_ts) <- names_cs
        for(cs in 1:n_cs){
          demand_window_ts_train <- demand_window_train[[ts]][,cs]
          demand_ival = demand_window_ts_train[(ival_last/miter - window_hours/miter + 1) : (ival_last/miter)] # for remember for later!!
          sarima_out <-  fit_sarima_own(y = demand_ival, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                        horizon = horizonVal, fourier_dummy = fourier_dummy,
                                        optimizeK = optimizeK)
          fcst_ival_ts[,cs] = sarima_out$forecast
        }
        fcst_val_ts <- rbind(forecasts_val_old[[ts]][(horizonVal/miter+1):(validation/miter), ], fcst_ival_ts)
        forecasts_val[[ts]] <- fcst_val_ts
        timing_arima <- proc.time() - ptm_arima_val
        cat("validation fit sarima at frequency",miter, "takes", timing_arima[3], "seconds", "\n")
      }
    }
  }
  
  return(forecasts_val)
}





fit_base_forecast_oos <- function(x, base_method = "naive", ts_aggr_orders, seasonal_frequency = NULL,
                                  window_hours, Whour, horizon, oos_indices_fix, 
                                  n_cs, names_cs, 
                                  fourier_dummy = FALSE, optimizeK = TRUE, Kfourier = NULL){
  n_ts = length(ts_aggr_orders)
  demand_window_insample = x
  
  if(base_method == "naive"){
    #### Naive Last Week ####
    base_forecasts_oos_iwindow <- residuals_insample_iwindow <- vector(mode = "list", length = n_ts)
    names(base_forecasts_oos_iwindow) <- names(residuals_insample_iwindow) <- paste0("m",ts_aggr_orders)
    for(ts in 1:n_ts){
      miter <- ts_aggr_orders[ts]
      fit_naive <- demand_window_insample[[ts]][-c(oos_indices_fix[[ts]] - (Whour/miter)),]
      resid_insample <- demand_window_insample[[ts]][-c(1:(Whour/miter)), ] - fit_naive
      fcst_oos = demand_window_insample[[ts]][oos_indices_fix[[ts]] - (Whour/miter), , drop = F] 
      if(class(fcst_oos)[1] == "numeric"){
        fcst_oos <- t(as.matrix(fcst_oos))
      }
      residuals_insample_iwindow[[ts]] <- resid_insample
      base_forecasts_oos_iwindow[[ts]] <- fcst_oos
    }
  }
  
  if(base_method == "ets"){
    #### ETS ###
    base_forecasts_oos_iwindow <- residuals_insample_iwindow <- vector(mode = "list", length = n_ts)
    names(base_forecasts_oos_iwindow) <- names(residuals_insample_iwindow) <- paste0("m",ts_aggr_orders)
    for(ts in 1:n_ts){
      ptm_ets_oos <- proc.time()
      miter = ts_aggr_orders[ts]
      fcst_oos_ts =  matrix(NA, nrow = horizon/miter, ncol = n_cs)
      resid_insample <-  matrix(NA, nrow = nrow(demand_window_insample[[ts]]), ncol = n_cs)
      colnames(fcst_oos_ts) <- colnames(resid_insample) <- names_cs
      for(cs in 1:n_cs){
        demand_window_ts_insample <- demand_window_insample[[ts]][,cs]
        ets_out <-  fit_ets_own(y = demand_window_ts_insample, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                horizon = horizon, fourier_dummy = fourier_dummy,
                                optimizeK = optimizeK)
        fcst_oos_ts[,cs] = ets_out$forecast
        resid_insample[,cs] = ets_out$residuals
      }
      residuals_insample_iwindow[[ts]] <- resid_insample
      base_forecasts_oos_iwindow[[ts]] <- fcst_oos_ts
      timing_ets <- proc.time() - ptm_ets_oos
      cat("test fit ets at frequency",miter, "takes", timing_ets[3], "seconds", "\n")
    }
  }
  
  if(base_method == "sarima"){
    #### SARIMA ####
    base_forecasts_oos_iwindow <- residuals_insample_iwindow <- vector(mode = "list", length = n_ts)
    names(base_forecasts_oos_iwindow) <- names(residuals_insample_iwindow) <- paste0("m",ts_aggr_orders)
    for(ts in 1:n_ts){
      ptm_arima_oos <- proc.time()
      miter = ts_aggr_orders[ts]
      fcst_oos_ts =  matrix(NA, nrow = horizon/miter, ncol = n_cs)
      resid_insample <-  matrix(NA, nrow = nrow(demand_window_insample[[ts]]), ncol = n_cs)
      colnames(fcst_oos_ts) <- colnames(resid_insample) <- names_cs
      for(cs in 1:n_cs){
        demand_window_ts_insample <- demand_window_insample[[ts]][,cs]
        sarima_out <-  fit_sarima_own(y = demand_window_ts_insample, seas_freq = seasonal_frequency[ts], K = Kfourier[ts], m = miter,
                                      horizon = horizon, fourier_dummy = fourier_dummy,
                                      optimizeK = optimizeK)
        fcst_oos_ts[,cs] = sarima_out$forecast
        resid_insample[,cs] = sarima_out$residuals
      }
      residuals_insample_iwindow[[ts]] <- resid_insample
      base_forecasts_oos_iwindow[[ts]] <- fcst_oos_ts
      timing_arima <- proc.time() - ptm_arima_oos
      cat("test fit sarima at frequency",miter, "takes", timing_arima[3], "seconds", "\n")
    }
  }
  
  out = list(base_forecasts_oos = base_forecasts_oos_iwindow, residuals_insample = residuals_insample_iwindow)
  return(out)
  
}



## No tuning 
fit_ml_reconciliation <- function(inputlist_ML_base_forecasts, n_cs, n_cs_up, n_cs_bt, C, ts_aggr_orders, delivery_areas, 
                                  ML_method = "randomforest", 
                                  ML_loss = "squared", ML_predictor = "cstemp", tweedie_variance_power = 1.5, round_base = TRUE,
                                  seed = 2023, seed_mat = NULL){
  set.seed(seed)
  if(is.null(seed_mat)){
    seed_mat = sample(1:1000000, n_cs_bt, replace = F)
  }
  #seed_mat = matrix( sample(1:1000000, length(rolling_sequence)*n_cs_bt, replace = F), nrow = length(rolling_sequence), ncol = n_cs_bt)
  
  n_ts = length(ts_aggr_orders)
  
  recon_bt_forecasts = matrix(NA, nrow = nrow(inputlist_ML_base_forecasts$Xout), ncol = n_cs_bt)
  colnames(recon_bt_forecasts) <- delivery_areas
  
  # apply ML to highest frequency (30min), bottom-level (delivery-area) forecasts (for each delivery area)
  for(csl in 1:n_cs_bt){
    # select correct predictors for ML
    # cross-temporal
    if(ML_predictor == "cstemp"){
      get_Xcols = c(1:n_cs, n_cs*c(1:(n_ts-1)) + n_cs_up + csl)
    }
    # cross-sectional
    if(ML_predictor == "cs"){
      get_Xcols = c(1:n_cs)
    }
    # temporal 
    if(ML_predictor == "temp"){
      get_Xcols = c(n_cs*c(0:(n_ts-1)) + n_cs_up + csl)
    }
    # all
    if(ML_predictor == "cstempall"){
      get_Xcols = c(1:(n_cs*n_ts))
    }
    y = inputlist_ML_base_forecasts$Y[,n_cs_up + csl]
    Xin = inputlist_ML_base_forecasts$Xin[,get_Xcols]
    Xout = inputlist_ML_base_forecasts$Xout[,get_Xcols]
    if(round_base){
      Xin <-  pmax(round(Xin),0)
      Xout <-  pmax(round(Xout),0)
    }
    
    set.seed(seed_mat[csl])
    # Train ML
    if(ML_method == "randomforest"){
      fit_RF_own <- fit_rf(y = y, X = Xin, Xout = Xout)
      recon_bt_forecasts[,csl] <- pmax(0,round(fit_RF_own$forecasts))
    }
    if(ML_method == "xgboost"){
      if(ML_loss == "squared"){
        fit_xgboost_own <- fit_xgboost(y = y, X = Xin, Xout = Xout)
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_xgboost_own$forecasts))
      }
      if(ML_loss == "tweedie"){
        fit_xgboost_own <- fit_xgboost_tweedie(y = y, X = Xin, Xout = Xout, tweedie_variance_power = tweedie_variance_power)
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_xgboost_own$forecasts))
      }
    }
    if(ML_method == "lightgbm"){
      if(ML_loss == "squared"){
        fit_lightgbm_own <- fit_lightgbm(y = y, X = Xin, Xout = Xout)
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_lightgbm_own$forecasts))
      }
      if(ML_loss == "tweedie"){
        fit_lightgbm_own <- fit_lightgbm_tweedie(y = y, X = Xin, Xout = Xout,tweedie_variance_power = tweedie_variance_power)
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_lightgbm_own$forecasts))
      }
    }
    if(ML_method == "bottomup"){
      recon_bt_forecasts[,csl] <- pmax(0,round(Xout[,n_cs_up + csl]))
    }
  }
  # Reconstruct the other hierarchical levels of the forecast (aggregate from the bottom level 30-min, area data)
  # Step 1: cross-sectional aggregation on highest-frequency level (30-min)
  upper_cs = as.matrix(recon_bt_forecasts) %*% t(C)
  recon_cs = cbind(upper_cs, as.matrix(recon_bt_forecasts))
  # Step 2: temporal aggregation
  recon_csts_forecasts <- aggregate_ts_hierachy(recon_cs, ts_aggr_orders)
  
  return(recon_csts_forecasts)
  
}

fit_ml_reconciliation_tuning <- function(inputlist_ML_base_forecasts, n_cs, n_cs_up, n_cs_bt, C, ts_aggr_orders, delivery_areas, 
                                         ML_method = "randomforest", 
                                         ML_loss = "squared", ML_predictor = "cstemp", round_base = TRUE,
                                         seed = 2023){
  set.seed(seed)
  seed_mat = sample(1:1000000, n_cs_bt, replace = F)
  n_ts = length(ts_aggr_orders)
  
  recon_bt_forecasts = matrix(NA, nrow = nrow(inputlist_ML_base_forecasts$Xout), ncol = n_cs_bt)
  colnames(recon_bt_forecasts) <- delivery_areas
  
  params_list = vector(mode = "list", length = n_cs_bt)
  names(params_list) = delivery_areas
  errors_BayesOpt_flag = rep(0, n_cs_bt)
  
  # apply ML to highest frequency (30min), bottom-level (delivery-area) forecasts (for each delivery area)
  for(csl in 1:n_cs_bt){
    # select correct predictors for ML
    # cross-temporal
    if(ML_predictor == "cstemp"){
      get_Xcols = c(1:n_cs, n_cs*c(1:(n_ts-1)) + n_cs_up + csl)
    }
    # cross-sectional
    if(ML_predictor == "cs"){
      get_Xcols = c(1:n_cs)
    }
    # temporal 
    if(ML_predictor == "temp"){
      get_Xcols = c(n_cs*c(0:(n_ts-1)) + n_cs_up + csl)
    }
    # all
    if(ML_predictor == "cstempall"){
      get_Xcols = c(1:(n_cs*n_ts))
    }
    y = inputlist_ML_base_forecasts$Y[,n_cs_up + csl]
    Xin = inputlist_ML_base_forecasts$Xin[,get_Xcols]
    Xout = inputlist_ML_base_forecasts$Xout[,get_Xcols]
    if(round_base){
      Xin <-  pmax(round(Xin),0)
      Xout <-  pmax(round(Xout),0)
    }
    
    
    if(ML_method == "randomforest"){
      ptms_tune = proc.time()
      params_rf = parameter_tuning_rf(y = y, X = Xin, Dhour = Dhour, days = validationDays, seed = seed_mat[csl])
      params = params_rf$OPT_Res
      params_list[[csl]] = params
      ptme_tune = proc.time() - ptms_tune
      cat("parameter tuning of delivery area", csl ,"takes", ptme_tune[3], "seconds", "\n") #
      
      # fit to tuned parameters
      set.seed(seed_mat[csl])
      fit_rf_own <- fit_rf(y = y, X = Xin, Xout = Xout, parlist = as.list(params))
      recon_bt_forecasts[,csl] <- pmax(0,round(fit_rf_own$forecasts))
    }
    if(ML_method == "xgboost"){
      if(ML_loss == "squared"){
        ptms_tune = proc.time()
        params_xgb = parameter_tuning_xgb(y = y, X = Xin, Dhour = Dhour, days = validationDays, seed = seed_mat[csl])
        if(class(params_xgb$OPT_Res) == "try-error"){
          errors_BayesOpt_flag[csl] = 1 # error flag 
          params = unlist(lapply(params_xgb$bounds, mean))
          params_list[[csl]] = params
        }else{ # update parameters
          params = params_xgb$OPT_Res$Best_Par
          params_list[[csl]] = params
        }
        ptme_tune = proc.time() - ptms_tune
        cat("parameter tuning of delivery area", csl ,"takes", ptme_tune[3], "seconds", "\n") #
        # fit to tuned parameters
        set.seed(seed_mat[csl])
        fit_xgboost_own <- fit_xgboost(y = y, X = Xin, Xout = Xout, parlist = as.list(params))
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_xgboost_own$forecasts))
      }
      if(ML_loss == "tweedie"){
        ptms_tune = proc.time()
        params_xgb = parameter_tuning_xgb_tweedie(y = y, X = Xin, Dhour = Dhour, days = validationDays, seed = seed_mat[csl])
        if(class(params_xgb$OPT_Res) == "try-error"){
          errors_BayesOpt_flag[csl] = 1 # error flag 
          params = unlist(lapply(params_xgb$bounds, mean))
          params_list[[csl]] = params
        }else{
          params = params_xgb$OPT_Res$Best_Par
          params_list[[csl]] = params
        }
        ptme_tune = proc.time() - ptms_tune
        cat("parameter tuning of delivery area", csl ,"takes", ptme_tune[3], "seconds", "\n") #
        # fit to tuned parameters
        set.seed(seed_mat[csl])
        fit_xgboost_own <- fit_xgboost_tweedie(y = y, X = Xin, Xout = Xout, parlist = as.list(params))
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_xgboost_own$forecasts))
      }
    }
    if(ML_method == "lightgbm"){
      if(ML_loss == "squared"){
        ptms_tune = proc.time()
        params_lgbm = parameter_tuning_lgbm(y = y, X = Xin, Dhour = Dhour, days = validationDays, seed = seed_mat[csl])
        if(class(params_lgbm$OPT_Res) == "try-error"){
          errors_BayesOpt_flag[csl] = 1 # error flag 
          params = unlist(lapply(params_lgbm$bounds, mean))
          params_list[[csl]] = params
        }else{
          params = params_lgbm$OPT_Res$Best_Par
          params_list[[csl]] = params
        }
        ptme_tune = proc.time() - ptms_tune
        cat("parameter tuning of delivery area", csl ,"takes", ptme_tune[3], "seconds", "\n") #
        # fit to tuned parameters
        set.seed(seed_mat[csl])
        fit_lightgbm_own <- fit_lightgbm(y = y, X = Xin, Xout = Xout, parlist = as.list(params))
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_lightgbm_own$forecasts))
      }
      if(ML_loss == "tweedie"){
        ptms_tune = proc.time()
        params_lgbm = parameter_tuning_lgbm_tweedie(y = y, X = Xin, Dhour = Dhour, days = validationDays, seed = seed_mat[csl])
        if(class(params_lgbm$OPT_Res) == "try-error"){
          errors_BayesOpt_flag[csl] = 1 # error flag 
          params = unlist(lapply(params_lgbm$bounds, mean))
          params_list[[csl]] = params
        }else{
          params = params_lgbm$OPT_Res$Best_Par
          params_list[[csl]] = params
        }
        ptme_tune = proc.time() - ptms_tune
        cat("parameter tuning of delivery area", csl ,"takes", ptme_tune[3], "seconds", "\n") #
        # fit to tuned parameters
        set.seed(seed_mat[csl])
        fit_lightgbm_own <- fit_lightgbm_tweedie(y = y, X = Xin, Xout = Xout, parlist = as.list(params))
        recon_bt_forecasts[,csl] <- pmax(0,round(fit_lightgbm_own$forecasts))
      }
    }
  }
  # Reconstruct the other hierarchical levels of the forecast (aggregate from the bottom level 30-min, area data)
  # Step 1: cross-sectional aggregation on highest-frequency level (30-min)
  upper_cs = as.matrix(recon_bt_forecasts) %*% t(C)
  recon_cs = cbind(upper_cs, as.matrix(recon_bt_forecasts))
  # Step 2: temporal aggregation
  recon_csts_forecasts <- aggregate_ts_hierachy(recon_cs, ts_aggr_orders)
  
  out = list("recon_csts_forecasts" = recon_csts_forecasts, "params_best" = params_list, "errors_BayesOpt_flag" = errors_BayesOpt_flag )
  return(out)
  
}







