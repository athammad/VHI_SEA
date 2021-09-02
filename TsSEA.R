library(modeltime)
library(timetk)
library(tidymodels)
library(data.table)

#chnage name of variables (it seems that is better to use the same names they use in the library)
setnames(SeaTS,c("CRN","Date","VHI"),c("id","date","value"))
SeaTS$id<-as.factor(SeaTS$id)

#con Recursive no multivariate (unless with a VAR model or system of equations)
#check:
#https://stats.stackexchange.com/questions/530328/recursive-time-series-forecasting-model
SeaTS<-SeaTS[,.(id,date,value)]
names(SeaTS)
setDF(SeaTS)


SeaTS %>%  
  plot_time_series(
    .date_var    = date, 
    .value       = value, 
    .facet_var   = id, 
    .facet_ncol  = 2,
    .smooth      = F, 
    .interactive = F
  )

####################################################################################################################
# MODELLING TIME SERIES WITH:
# RECURSIVE APPROACH
# TIME SLICE CV

FORECAST_HORIZON <- 52*2
LAGS<-52
#####################################################################################################################



ts_extended <- SeaTS %>%
  group_by(id) %>%
  future_frame(
    .length_out = FORECAST_HORIZON,
    .bind_data  = TRUE
  ) %>%
  ungroup()


lag_roll_transformer_grouped <- function(data){
  data %>%
    group_by(id) %>%
    tk_augment_lags(value, .lags = 1:LAGS) %>%
    tk_augment_slidify(
      .value   = contains("lag12"),
      .f       = ~mean(.x, na.rm = T),
      .period  = c(12,24,26),
      .partial = TRUE
    ) %>%
    ungroup()
}

ts_lags <- ts_extended %>%
  lag_roll_transformer_grouped()

ts_lags
names(ts_lags)


train_data <- ts_lags %>%
  drop_na()

future_data <- ts_lags %>%
  filter(is.na(value))


ts_resamples <-train_data %>%
  time_series_cv(
    date_var    = date, 
    assess      = "3 years",
    skip        = "1 year",
    cumulative  = TRUE,
    slice_limit = 6
  )

ts_resamples %>%
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(.date_var = date, .value = value,.facet_ncol = 2,
                           .interactive = FALSE)



#DUBBIO QUI SU STEP TIMESEIRES SIGNATURE
recipe_parsnip <- recipe(value ~ ., training(ts_resamples$splits[[1]])) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_mutate(date_month = factor(date_month, ordered = TRUE)) %>%
  step_dummy(all_nominal(), one_hot = TRUE)%>%  
  update_role(id, new_role = "id variable") 



model_fit_rf_recursive_tune <- rand_forest(
  mode= "regression",
  mtry= tune(),
  trees= 1000,
  min_n= tune()) %>%
  set_engine("ranger") 

# boost_tree(
#   mode           = "regression",
#   mtry           = tune(),
#   trees          = 1000,
#   min_n          = tune(),
#   tree_depth     = tune(),
#   learn_rate     = tune(),
#   loss_reduction = tune(),
#   sample_size    = tune()
# ) %>%
#   set_engine("xgboost")

wflw <- workflow() %>%
  add_model(model_fit_rf_recursive_tune) %>%
  add_recipe(recipe_parsnip)



#parallel_start(type="FORK",10)# paralle non va most of the time!!!!

tune_results_rf <- tune_grid(
  object     = wflw,
  resamples  = ts_resamples,
  param_info = parameters(wflw),
  grid       = 5,
  control    = control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = "everything")
)




rf_tuned_best <- tune_results_rf %>%
  select_best("rmse")

fin_wflw <- wflw %>%
  finalize_workflow(parameters = rf_tuned_best)


# ---- train with Best Hype ----

wflw_fit <- fin_wflw %>%
  fit(training(ts_resamples$splits[[1]]))%>% 
  recursive(
    id= "id", 
    transform = lag_roll_transformer_grouped,
    train_tail = panel_tail(training(ts_resamples$splits[[1]]),id, FORECAST_HORIZON)# might not need to put the ID var as string
  )

modeltime_tbl <- wflw_fit %>% modeltime_table()
#parallel_stop(type="FORK")# doubts!!!!!salva, chiudi e riparti... nn chiude i cores
# ---- ACCURACY ON THE TRAINING ----

#????????????

# ---- CALIBRATE ---- (necessery for pred interval)
calibration_tbl <- modeltime_tbl%>% 
  modeltime_calibrate(
    new_data =testing(ts_resamples$splits[[1]]),
    id = "id",quiet = FALSE, keep_data  = TRUE
  )

# ---- PREDICT ON THE TEST ---- (here I would like to see the plots with actual and predicted)

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(ts_resamples$splits[[1]]), 
    actual_data =train_data,
    keep_data   = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .interactive        = FALSE,
    .conf_interval_show = TRUE,
    .facet_ncol         = 2
  )

# ---- ACCURACY ON THE TEST ---- ((here I would like to see the metrics by ID)
#????? NON GLI PIACE ID 
calibration_tbl %>%
  modeltime_accuracy(
    new_data = testing(ts_resamples$splits[[1]]),
    acc_by_id = T) %>%
  table_modeltime_accuracy(.interactive = FALSE)


# ---- REFIT ON ALL THE DATA ----

refit_tbl <- calibration_tbl %>% 
  modeltime_refit(
    data = train_data,
    control = control_refit(allow_par = F,verbose = T)
  )

# ---- ACCURACY ON THE TRAINING refitted ----

#????????????

# ---- PREDICT IN THE FUTURE ----

nested_data <- refit_tbl %>%
  modeltime_forecast(
    new_data = future_data,
    actual_data = train_data,
    keep_data = TRUE
  )%>%
  group_by(id) %>%
  plot_modeltime_forecast(.interactive = FALSE, .facet_ncol = 2)

nested_data


#parallel_stop()


#TO DO
#GET METRICS ON THE FIRST TRAINING
#GET METRICS BY ID ON THE TEST
#GET METRICS ON THE SECOND TRAINING (i.e the Refit)
