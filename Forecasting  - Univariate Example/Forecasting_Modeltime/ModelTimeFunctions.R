### Modeltime Functions

initialTimeSeriesEval <- function(df = dts,
                                  response = resp,
                                  predictor = pred) {
  
  initPlot <- df %>%
    timetk::plot_time_series(Date, Value, .interactive = FALSE)
  
  seasonality <- df %>%
    timetk::plot_seasonal_diagnostics(
      Date, Value,
      .feature_set = c("week", "month.lbl"),
      .interactive = FALSE
    )
  
  acf <- df %>%
    timetk::plot_acf_diagnostics(
      Date, Value,
      .interactive = FALSE,
      .show_white_noise_bars = TRUE)  +
    labs(title = "Lag Diagnostics")
  
  
  output <- list(
    "seasonality" = seasonality,
    "acf" = acf,
    "ts" = initPlot
  )
  
  return(output)
}

modelTimeSeries <- function(train = tr,
                            test = ts,
                            df = dts,
                            response = resp,
                            predictor = pred,
                            folds = cvFolds,
                            days = 7) {
  
  #Automate formula
  formulaBase <- stats::as.formula(paste(response, ".", sep="~"))
  
  #Recipe-free models
  arima_model <- modeltime::arima_reg() %>%
    parsnip::set_engine("auto_arima") %>%
    parsnip::fit(formulaBase, data = train)
  
  prophet_model <- modeltime::prophet_reg() %>%
    parsnip::set_engine("prophet") %>%
    parsnip::fit(formulaBase, data = train)
  
  ets_model <- modeltime::exp_smoothing() %>%
    parsnip::set_engine("ets") %>%
    parsnip::fit(formulaBase, data = train)
  
  tbats_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("tbats") %>%
    parsnip::fit(formulaBase, data = train)
  
  nn_model <- modeltime::nnetar_reg() %>%
    parsnip::set_engine("nnetar") %>%
    parsnip::fit(formulaBase, data = train)
  
  stlm_ets_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("stlm_ets") %>%
    parsnip::fit(formulaBase, data = train)
  
  stlm_arima_model <- modeltime::seasonal_reg() %>%
    parsnip::set_engine("stlm_arima") %>%
    parsnip::fit(formulaBase, data = train)
  
  # forecast_table <- modeltime::modeltime_table(
  #   arima_model,
  #   prophet_model,
  #   ets_model,
  #   tbats_model,
  #   nn_model,
  #   stlm_ets_model,
  #   stlm_arima_model
  # )
  
  #Models with recipes included
  rec <- recipes::recipe(Value ~ Date, train) %>%
    timetk::step_timeseries_signature(Date) %>%
    recipes::step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                     contains("second"), contains("xts")) %>%
    recipes::step_dummy(all_nominal()) %>%
    recipes::prep()
  
  #Elastic Net
  glmnet_model <- parsnip::linear_reg(penalty = 0.01, mixture = 0.67) %>%
    parsnip::set_engine("glmnet")
  
  glmnet_model_workflow <- workflows::workflow() %>%
    workflows::add_model(glmnet_model) %>%
    workflows::add_recipe(rec %>% step_rm(Date)) %>%
    parsnip::fit(train)
  
  #Prophet with XGBoost Errors
  prophet_boost_model <- modeltime::prophet_boost(seasonality_daily = "auto",
                                       trees = 100) %>%
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
    tbats_model,
    nn_model,
    stlm_ets_model,
    stlm_arima_model,
    glmnet_model_workflow
  )
  
  acc <- forecast_table %>%
    modeltime::modeltime_calibrate(test) %>%
    modeltime::modeltime_accuracy() %>%
    dplyr::arrange(mase)
  
  order <- acc$.model_id
  
  forecast_table <- forecast_table[order,]
  
  crossValPlot <- forecast_table %>%
    modeltime::modeltime_calibrate(test) %>%
    modeltime::modeltime_forecast(actual_data = test) %>%
    modeltime::plot_modeltime_forecast()
  
  fcastVals <- forecast_table %>%
    modeltime::modeltime_refit(df) %>%
    modeltime::modeltime_calibrate(df) %>%
    modeltime::modeltime_forecast(h = days, actual_data = df)
  
  fcast <- fcastVals %>%
    modeltime::plot_modeltime_forecast()
  
  #Model ensembling
  ensembleAvg <- forecast_table %>%
    modeltime.ensemble::ensemble_average(type = "mean")
  
  ensembleMed <- forecast_table %>%
    modeltime.ensemble::ensemble_average(type = "median")
  
  ensembleWeighted <- forecast_table[1:5,] %>%
    modeltime.ensemble::ensemble_weighted(loadings = c(5, 4, 3, 2, 1),
                                          scale_loadings = TRUE)
  
  ensembleTable <- modeltime::modeltime_table(
    ensembleAvg,
    ensembleMed,
    ensembleWeighted
  )
  
  accEnsemble <- ensembleTable %>%
    modeltime::modeltime_calibrate(test) %>%
    modeltime::modeltime_accuracy()
  
  fcastEnsembleVals <- ensembleTable %>%
    modeltime_calibrate(df) %>%
    modeltime_forecast(
      new_data    = test,
      actual_data = df
    )
  
  fcastEnsembleCVPlot <- fcastEnsembleVals %>%
    modeltime::plot_modeltime_forecast()
  
  fcastEnsembleFcast <- ensembleTable %>%
    modeltime::modeltime_refit(df) %>%
    modeltime::modeltime_calibrate(df) %>%
    modeltime::modeltime_forecast(h = days, actual_data = df)
  
  fcastEnsembleFcastPlot <- fcastEnsembleFcast %>%
    modeltime::plot_modeltime_forecast()
  
  output <- list(
    "accuracyTable" = acc,
    "crossValPlot" = crossValPlot,
    "forecastTable" = forecast_table,
    "forecastPlot" = fcast,
    "forecastValues" = fcastVals,
    "accuracyEnsembleTable" = accEnsemble,
    "ensembleCrossValPlot" = fcastEnsembleCVPlot,
    "ensembleForecastValues" = fcastEnsembleFcast,
    "ensembleForecastPlot" = fcastEnsembleFcastPlot
  )
  
  return(output)
  
}

resampleTimeSeries <- function(df = dts,
                               table = output$forecastTable,
                               sliceLimit = 5) {
  resamples <- modeltime.resample::time_series_cv(
    data        = df,
    assess      = floor(nrow(data) / 10),
    initial     = floor(nrow(data) / 4),
    skip        = floor(nrow(data) / 10),
    slice_limit = sliceLimit
  )
  
  # Begin with a Cross Validation Strategy
  resampleSplits <- resamples %>%
    modeltime.resample::tk_time_series_cv_plan() %>%
    modeltime.resample::plot_time_series_cv_plan(Date, Value, .facet_ncol = 2, .interactive = FALSE)
  
  resamplesFitted <- table %>%
    modeltime.resample::modeltime_fit_resamples(
      resamples = resamples,
      control   = tune::control_resamples(verbose = FALSE)
    )
  
  resamplesPlot <- resamplesFitted %>%
    modeltime.resample::plot_modeltime_resamples(
      .point_size  = 3,
      .point_alpha = 0.8,
      .interactive = FALSE
    )
  
  resamplesTable <- resamplesFitted %>%
    modeltime.resample::modeltime_resample_accuracy(summary_fns = mean) %>%
    modeltime::table_modeltime_accuracy(.interactive = FALSE)
  
  output <- list(
    "table" = resamplesTable,
    "plot" = resamplesPlot,
    "fit" = resamplesFitted,
    "splits" = resampleSplits
  )
  
  return(output)
  
}
