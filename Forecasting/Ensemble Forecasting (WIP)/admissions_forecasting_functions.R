smooth_anomalies <- function(data = df) {
  
  #Compute anomalies
  df_anom <- data %>%
    timetk::tk_anomaly_diagnostics(Date, Value)
  
  # df_anom <- df_anom %>%
  #   mutate(observed = if_else(Anomaly == "Yes", ))
  
  if(df_anom$Anomaly == "Yes") {
    #If the anomaly is low, then replace with lower bound
    if(df_anom$observed < df_anom$recomposed_l1) {
      df_anom$observed <-  df_anom$recomposed_l1
    } else { #replace with higher bound
      df_anom$observed <-  df_anom$recomposed_l2
    }
    data$Value <- df_anom$observed
    
    return(data)
  }
}

forecast_ensemble_model <- function(df = dts,
                                    response = resp,
                                    var = var_name,
                                    predictor = pred,
                                    is_grouped_by = TRUE,
                                    period = 28) {
  
  if(is_grouped_by == TRUE) {
    df <- df %>%
      filter(ID == var) %>%
      dplyr::select(-ID) 
  }
  
  # df_orig <- df
  # lambda <- forecast::BoxCox.lambda(df$Value)
  # df$Value <- BoxCox(df$Value, lambda)
  
  #Prepare data
  split <- initial_time_split(df, prop = (nrow(df) - period) / nrow(df))
  train <- training(split)
  test <- testing(split)
  
  #Automate formula
  formula_base <- stats::as.formula(paste(response, ".", sep="~"))
  
  #Recipe-free models
  
  #Auto-ARIMA
  arima_model <- modeltime::arima_reg() %>%
    parsnip::set_engine("auto_arima") %>%
    parsnip::fit(formula_base, data = train)
  
  #Prophet
  prophet_model <- modeltime::prophet_reg() %>%
    parsnip::set_engine("prophet") %>%
    parsnip::fit(formula_base, data = train)
  
  #Automated Exponential Smoothing
  ets_model <- modeltime::exp_smoothing() %>%
    parsnip::set_engine("ets") %>%
    parsnip::fit(formula_base, data = train)
  
  #CROSTON - Special Case for Intermittent Demand
  croston_model <- modeltime::exp_smoothing() %>%
    parsnip::set_engine("croston") %>%
    parsnip::fit(formula_base, data = train)
  
  #Exponential Smoothing with Drift
  theta_model <- modeltime::exp_smoothing() %>%
    parsnip::set_engine("theta") %>%
    parsnip::fit(formula_base, data = train)
  
  #ADAM Univariate Regression
  adam_model <- modeltime::adam_reg() %>%
    parsnip::set_engine("auto_adam") %>%
    parsnip::fit(formula_base, data = train)
  
  #TBATS
  tbats_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("tbats") %>%
    parsnip::fit(formula_base, data = train)
  
  #Neural Net
  nn_model <- modeltime::nnetar_reg() %>%
    parsnip::set_engine("nnetar") %>%
    parsnip::fit(formula_base, data = train)
  
  #Seasonal Trend Loss Exp Smoothing
  stlm_ets_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("stlm_ets") %>%
    parsnip::fit(formula_base, data = train)
  
  #Seasonal Trend Loss ARIMA
  stlm_arima_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("stlm_arima") %>%
    parsnip::fit(formula_base, data = train)
  
  #Naive Model
  naive_model <- modeltime::naive_reg() %>%
    set_engine("naive") %>%
    fit(formula_base, data = train)
  
  #Seasonal naive Model
  seasonal_naive_model <- modeltime::naive_reg() %>%
    set_engine("snaive") %>%
    fit(formula_base, data = train)
  
  #THIEF model - Temporal Hierarchical Forecasting (Rob Hyndman)
  thief_model <- temporal_hierarchy() %>%
    set_engine("thief") %>%
    fit(formula_base, train)
  
  ###Bayesian Models from bayesmodels
  
  #Linear Additive State Space Model
  # add_ss_model <- additive_state_space() %>%
  #   set_engine("stan") %>%
  #   fit(formula_base, train)
  
  #Bayesian Structural Time Series (BSTS) gave errors when re-fitting
  
  #Stochastic Volatility Regression is too slow and error-prone, not included
  
  ###ML Models - with recipes included
  rec <- recipes::recipe(Value ~ Date, train) %>%
    timetk::step_timeseries_signature(Date) %>%
    timetk::step_fourier(Date, period = 52, K = 2) %>%
    recipes::step_holiday(Date, holidays = timeDate::listHolidays(pattern = "US")) %>%
    recipes::step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                     contains("second"), contains("xts")) %>%
    recipes::step_nzv(all_nominal_predictors()) %>%
    recipes::step_dummy(all_nominal()) %>%
    recipes::prep()
  
  #Elastic Net
  glmnet_model <- parsnip::linear_reg(penalty = 0.01, mixture = 0.67) %>%
    parsnip::set_engine("glmnet")
  
  glmnet_model_workflow <- workflows::workflow() %>%
    workflows::add_model(glmnet_model) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #MARS 
  mars_model <- mars(mode = "regression") %>%
    set_engine("earth") 
  
  mars_model_workflow <- workflows::workflow() %>%
    workflows::add_model(mars_model) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #ADAM Regression (Multivariate)
  # adam_model <- modeltime::adam_reg() %>%
  #   parsnip::set_engine("auto_adam")
  # 
  # adam_multivar_model_workflow <- workflows::workflow() %>%
  #   workflows::add_model(adam_model) %>%
  #   workflows::add_recipe(rec %>% step_rm(Date)) %>%
  #   parsnip::fit(train)
  
  ###XGBoost Models
  
  #Lower learn rate and mtry
  xg_model_.01_learn_5_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .01,
    mtry = 5
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.01_learn_5_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.01_learn_5_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #Medi learn rate and mtry
  xg_model_.1_learn_5_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .1,
    mtry = 5
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.1_learn_5_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.1_learn_5_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.3 learn rate, 5 mtry
  xg_model_.3_learn_5_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .3,
    mtry = 5
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.3_learn_5_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.3_learn_5_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.5 learn rate, 5 mtry
  xg_model_.5_learn_5_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .5,
    mtry = 5
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.5_learn_5_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.5_learn_5_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.65 learn rate, 5 mtry
  xg_model_.65_learn_5_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .65,
    mtry = 5
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.65_learn_5_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.65_learn_5_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.01 learn rate, 10 mtry
  xg_model_.01_learn_10_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .01,
    mtry = 10
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.01_learn_10_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.01_learn_10_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.1 learn, 10 mtry
  xg_model_.1_learn_10_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .1,
    mtry = 10
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.1_learn_10_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.1_learn_10_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.3 learn rate, 5 mtry
  xg_model_.3_learn_10_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .3,
    mtry = 10
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.3_learn_10_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.3_learn_10_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.5 learn rate, 10 mtry
  xg_model_.5_learn_10_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .5,
    mtry = 10
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.5_learn_10_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.5_learn_10_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #.65 learn rate, 5 mtry
  xg_model_.65_learn_10_mtry <- boost_tree(
    mode = "regression",
    learn_rate = .65,
    mtry = 10
  ) %>%
    set_engine("xgboost") 
  
  xg_model_workflow_.65_learn_10_mtry <- workflows::workflow() %>%
    workflows::add_model(xg_model_.65_learn_10_mtry) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #Prophet with XGBoost Errors
  prophet_boost_model <- modeltime::prophet_boost(
    seasonality_daily = "auto",
    trees = 100
  ) %>%
    parsnip::set_engine("prophet_xgboost")
  
  prophet_boost_model_workflow <- workflows::workflow() %>%
    workflows::add_model(prophet_boost_model) %>%
    workflows::add_recipe(rec) %>%
    parsnip::fit(train)
  
  #Auto ARIMA with XGBoost Errors
  auto_arima_boost_model <- modeltime::arima_boost() %>%
    parsnip::set_engine("auto_arima_xgboost")
  
  arima_boost_model_workflow <- workflows::workflow() %>%
    workflows::add_model(auto_arima_boost_model) %>%
    workflows::add_recipe(rec) %>%
    parsnip::fit(train)
  
  forecast_table <- modeltime_table(
    arima_model,
    prophet_model,
    ets_model,
    croston_model,
    theta_model,
    adam_model,
    tbats_model,
    nn_model,
    stlm_ets_model,
    stlm_arima_model,
    naive_model,
    seasonal_naive_model,
    thief_model,
    #add_ss_model,
    glmnet_model_workflow,
    mars_model_workflow,
    xg_model_workflow_.01_learn_5_mtry,
    xg_model_workflow_.1_learn_5_mtry,
    xg_model_workflow_.3_learn_5_mtry,
    xg_model_workflow_.5_learn_5_mtry,
    xg_model_workflow_.65_learn_5_mtry,
    xg_model_workflow_.01_learn_10_mtry,
    xg_model_workflow_.1_learn_10_mtry,
    xg_model_workflow_.3_learn_10_mtry,
    xg_model_workflow_.5_learn_10_mtry,
    xg_model_workflow_.65_learn_10_mtry,
    prophet_boost_model_workflow,
    arima_boost_model_workflow
  )
  
  acc <- forecast_table %>%
    modeltime::modeltime_calibrate(test) %>%
    modeltime::modeltime_accuracy() %>%
    dplyr::arrange(mase)
  
  order <- acc$.model_id
  
  forecast_table <- forecast_table[order,]

  #Model ensembling - top 3
  ensemble_avg_3 <- forecast_table[1:3,] %>%
    modeltime.ensemble::ensemble_average(type = "mean")
  
  ensemble_med_3 <- forecast_table[1:3,] %>%
    modeltime.ensemble::ensemble_average(type = "median")
  
  ensemble_weighted_3 <- forecast_table[1:3,] %>%
    modeltime.ensemble::ensemble_weighted(loadings = c(3, 2, 1),
                                          scale_loadings = TRUE)
  
  #Model ensembling - top 5
  ensemble_avg_5 <- forecast_table[1:5,] %>%
    modeltime.ensemble::ensemble_average(type = "mean")
  
  ensemble_med_5 <- forecast_table[1:5,] %>%
    modeltime.ensemble::ensemble_average(type = "median")
  
  ensemble_weighted_5 <- forecast_table[1:5,] %>%
    modeltime.ensemble::ensemble_weighted(loadings = c(5, 4, 3, 2, 1),
                                          scale_loadings = TRUE)
  
  ensemble_table <- modeltime::modeltime_table(
    ensemble_avg_3,
    ensemble_med_3,
    ensemble_weighted_3,
    ensemble_avg_5,
    ensemble_med_5,
    ensemble_weighted_5
  ) 
  
  ensemble_acc <- ensemble_table %>%
    modeltime::modeltime_calibrate(test) %>%
    modeltime::modeltime_accuracy() %>%
    dplyr::arrange(mase)
  
  order_en <- ensemble_acc$.model_id
  
  ensemble_table <- ensemble_table[order_en,]
  
  # ensemble_vals <- ensemble_table %>%
  #   modeltime_calibrate(df) %>%
  #   modeltime_forecast(
  #     new_data    = test,
  #     actual_data = df
  #   )
  # 
  # ensemble_cv <- ensemble_vals %>%
  #   modeltime::plot_modeltime_forecast()
  
  ensemble_forecast_95 <- ensemble_table[1,] %>%
    modeltime::modeltime_refit(df) %>%
    modeltime::modeltime_calibrate(df) %>%
    modeltime::modeltime_forecast(h = period, actual_data = df)

  ensemble_forecast_68 <- ensemble_table[1,] %>%
    modeltime::modeltime_refit(df) %>%
    modeltime::modeltime_calibrate(df) %>%
    modeltime::modeltime_forecast(h = period, actual_data = df, conf_interval = 0.68) %>%
    dplyr::select(.index, .conf_lo, .conf_hi) %>%
    set_names(".index", "conf_lo_1sd", "conf_hi_1sd")
  
  ensemble_forecast <- ensemble_forecast_95 %>%
    left_join(ensemble_forecast_68, by = c(".index" = ".index"))
  
  
  output <- list("ensemble_forecast" = ensemble_forecast)
  
  return(output)
}

ensemble_forecasts_grouped <- function(data = df,
                                       var = var_name,
                                       date_group = date,
                                       pct_cutoff = .01) {
  
  var <- ensym(var)
  date_group <- ensym(date_group)
  
  metric <- data %>%
    group_by(!!var) %>%
    tally() %>%
    mutate(pct = n / sum(n)) %>%
    filter(pct > pct_cutoff) %>%
    dplyr::select(!!var) %>%
    pull()
  
  df_nest <- data %>%
    group_by(!!date_group, !!var) %>%
    tally() %>%
    set_names("Date", "ID", "Value") %>%
    relocate(ID, .after = Value) %>%
    arrange(ID) %>%
    mutate(ID = as.factor(ID)) %>%
    as_tbl_time(index = Date)

  output <- pmap(list(var = metric), forecast_ensemble_model, df_nest)
  names(output) <- metric
  
  return(output)
}

forecast_plot <- function(data = df, plot_title = "IP Daily Admits") {
  p <- data %>%
    plot_modeltime_forecast(
      .title = plot_title,
      .x_lab = "Date", 
      .y_lab = "Admits",
      .interactive = FALSE
    ) +
    geom_ribbon(aes(ymin = conf_lo_1sd, ymax = conf_hi_1sd), alpha = .3) +
    theme_gray() +
    theme(legend.position = "bottom")
  
  return(p)
}