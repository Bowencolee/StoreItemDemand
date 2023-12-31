##
## Store Item Demand
##

library(tidymodels)
library(vroom)
library(patchwork)
library(modeltime) #time series tidymodels
library(timetk) # time series plots
library(embed) # target encoding
library(discrim) # naive bayes
library(ranger) # random forests
library(forecast) # ARIMA models


### DATA ###
# setwd("C:/Users/bowen/Desktop/Stat348/StoreItemDemand")
store_train <- vroom::vroom("train.csv")
store_test <- vroom::vroom("test.csv")


##### EDA ######
acf11 <- store_train[store_train$item == 1 & store_train$store == 1, ]
acf66 <- store_train[store_train$item == 6 & store_train$store == 6, ]
acf711 <- store_train[store_train$item == 11 & store_train$store == 7, ]
acf1050 <- store_train[store_train$item == 50 & store_train$store == 10, ]


plot1 <- acf11 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 1, Store 1")
plot2 <- acf66 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 6, Store 6")
plot3 <- acf711 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 11, Store 7")
plot4 <- acf1050 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 50, Store 10")

(plot1 + plot2) / (plot3 + plot4)


##### Predict #####
# nStores <- max(store_train$store)
# nItems <- max(store_train$item)
# for(s in 1:nStores){
#   for(i in 1:nItems){
#     storeItemTrain <- store_train %>%
#     filter(store==s, item==i)
#     storeItemTest <- store_test %>%
#     filter(store==s, item==i)
#     
#     ## Fit storeItem models here
#     
#     ## Predict storeItem sales
#     
#     ## Save storeItem predictions
#     if(s==1 & i==1){
#       all_preds <- preds
#     } else {
#       all_preds <- bind_rows(all_preds, preds)
#     }
#     
#   }
# }
##### RECIPE MAKING #####

my_recipe <- recipe(sales~., data=item_train) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))
  
  
prepped_recipe <- prep(my_recipe)
baked_recipe <- bake(prepped_recipe, item_train)


##### Random Forests #####
forest_model <- rand_forest(mtry = tune(), # how many var are considered
                            min_n=tune(), # how many observations per leaf
                            trees=1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

## Set Workflow
forest_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(forest_model)

## Grid of values to tune over
tuning_grid <- grid_regular(mtry(range =c(1,7)),
                            min_n(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(item_train, v = 5, repeats=1)

## Run the CV
CV_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape)) #Or leave metrics NULL

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best("smape")

mean <- collect_metrics(CV_results) %>%
  filter(mtry == bestTune$mtry, min_n == bestTune$min_n, .config == bestTune$.config)
mean 



## Finalize the Workflow & fit it
final_wf <-forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=item_train)

##### Exponential Smoothing #####
train749 <- store_train %>%
  filter(store==7, item==49)
test749 <- store_test %>%
  filter(store==7, item==49)

train313 <- store_train %>%
  filter(store==3, item==13)
test313 <- store_test %>%
  filter(store==3, item==13)

cv_split749 <- time_series_split(train749, assess="3 months", cumulative = TRUE)
cv_split313 <- time_series_split(train313, assess="3 months", cumulative = TRUE)

cv_split749 %>%
 tk_time_series_cv_plan() %>% #Put into a data frame
 plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split313 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


### 7/49 ###
es_model749 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split749))

## Cross-validate to tune model
cv_results749 <- modeltime_calibrate(es_model749,
                                  new_data = testing(cv_split749))

## Visualize CV results
p1 <- cv_results749 %>%
 modeltime_forecast(
    new_data = testing(cv_split749),
    actual_data = train749) %>%
 plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results749 %>%
 modeltime_accuracy() %>%
 table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
es_fullfit749 <- cv_results749 %>%
  modeltime_refit(data = train749)

es_preds749 <- es_fullfit749 %>%
 modeltime_forecast(h = "3 months") %>%
 rename(date=.index, sales=.value) %>%
 select(date, sales) %>%
 full_join(., y=test749, by="date") %>%
 select(id, sales)

p3 <- es_fullfit749 %>%
 modeltime_forecast(h = "3 months", actual_data = train749) %>%
 plot_modeltime_forecast(.interactive=FALSE)


### 3/13 ###
es_model313 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split313))

## Cross-validate to tune model
cv_results313 <- modeltime_calibrate(es_model313,
                                     new_data = testing(cv_split313))

## Visualize CV results
p2 <- cv_results313 %>%
  modeltime_forecast(
    new_data = testing(cv_split313),
    actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results313 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
es_fullfit313 <- cv_results313 %>%
  modeltime_refit(data = train313)

es_preds313 <- es_fullfit313 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test313, by="date") %>%
  select(id, sales)

p4 <- es_fullfit313 %>%
  modeltime_forecast(h = "3 months", actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=FALSE)



plotly::subplot(p1,p2,p3,p4, nrows = 2)

##### ARIMA Models #####
train749 <- store_train %>%
  filter(store==7, item==49)
test749 <- store_test %>%
  filter(store==7, item==49)

train313 <- store_train %>%
  filter(store==3, item==13)
test313 <- store_test %>%
  filter(store==3, item==13)

cv_split749 <- time_series_split(train749, assess="3 months", cumulative = TRUE)
cv_split313 <- time_series_split(train313, assess="3 months", cumulative = TRUE)

recipe749 <- recipe(sales~., data=train749) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

recipe313 <- recipe(sales~., data=train313) %>%
  step_rm(store, item) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year") %>%
  step_date(date, features="doy") %>%
  step_date(date, features="decimal") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))


arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
  set_engine("auto_arima")

arima_wf749 <- workflow() %>%
  add_recipe(recipe749) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split749))

arima_wf313 <- workflow() %>%
  add_recipe(recipe313) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split313))


cv_split749 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split313 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


### 7/49 ###
## Cross-validate to tune model
cv_results749 <- modeltime_calibrate(arima_wf749,
                                     new_data = testing(cv_split749))

## Visualize CV results
p1 <- cv_results749 %>%
  modeltime_forecast(
    new_data = testing(cv_split749),
    actual_data = train749) %>%
  plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results749 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
arima_fullfit749 <- cv_results749 %>%
  modeltime_refit(data = train749)

arima_preds749 <- arima_fullfit749 %>%
  modeltime_forecast(new_data = test749) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test749, by="date") %>%
  select(id, sales)

p3 <- arima_fullfit749 %>%
  modeltime_forecast(new_data = test749, actual_data = train749) %>%
  plot_modeltime_forecast(.interactive=FALSE)


### 3/13 ###
## Cross-validate to tune model
cv_results313 <- modeltime_calibrate(arima_wf313,
                                     new_data = testing(cv_split313))

## Visualize CV results
p2 <- cv_results313 %>%
  modeltime_forecast(
    new_data = testing(cv_split313),
    actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results313 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
arima_fullfit313 <- cv_results313 %>%
  modeltime_refit(data = train313)

arima_preds313 <- arima_fullfit313 %>%
  modeltime_forecast(new_data = test313) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test313, by="date") %>%
  select(id, sales)

p4 <- arima_fullfit313 %>%
  modeltime_forecast(new_data = test313, actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=FALSE)



plotly::subplot(p1,p2,p3,p4, nrows = 2)
##### Prophet Model #####
train749 <- store_train %>%
  filter(store==7, item==49)
test749 <- store_test %>%
  filter(store==7, item==49)

train313 <- store_train %>%
  filter(store==3, item==13)
test313 <- store_test %>%
  filter(store==3, item==13)

cv_split749 <- time_series_split(train749, assess="3 months", cumulative = TRUE)
cv_split313 <- time_series_split(train313, assess="3 months", cumulative = TRUE)

cv_split749 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split313 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

## 7/49 ##
prophet_model749 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split749))

## Cross-validate to tune model
cv_results749 <- modeltime_calibrate(prophet_model749,
                                     new_data = testing(cv_split749))

## Visualize CV results
p1 <- cv_results749 %>%
  modeltime_forecast(
    new_data = testing(cv_split749),
    actual_data = train749) %>%
  plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results749 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
prophet_fullfit749 <- cv_results749 %>%
  modeltime_refit(data = train749)

prophet_preds749 <- prophet_fullfit749 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test749, by="date") %>%
  select(id, sales)

p3 <- prophet_fullfit749 %>%
  modeltime_forecast(h = "3 months", actual_data = train749) %>%
  plot_modeltime_forecast(.interactive=FALSE)


### 3/13 ###
prophet_model313 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split313))

## Cross-validate to tune model
cv_results313 <- modeltime_calibrate(prophet_model313,
                                     new_data = testing(cv_split313))

## Visualize CV results
p2 <- cv_results313 %>%
  modeltime_forecast(
    new_data = testing(cv_split313),
    actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=F)

## Evaluate the accuracy
cv_results313 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit the data
prophet_fullfit313 <- cv_results313 %>%
  modeltime_refit(data = train313)

prophet_preds313 <- prophet_fullfit313 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test313, by="date") %>%
  select(id, sales)

p4 <- prophet_fullfit313 %>%
  modeltime_forecast(h = "3 months", actual_data = train313) %>%
  plot_modeltime_forecast(.interactive=FALSE)



plotly::subplot(p1,p2,p3,p4, nrows = 2)


##### Kaggle Submission #####
## Libraries
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

## Read in the data
item <- vroom::vroom("/kaggle/input/demand-forecasting-kernels-only/train.csv")
itemTest <- vroom::vroom("/kaggle/input/demand-forecasting-kernels-only/test.csv")
n.stores <- max(item$store)
n.items <- max(item$item)

## Define the workflow
item_recipe <- recipe(sales~., data=item) %>%
  step_date(date, features=c("dow", "month", "decimal", "doy", "year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())
boosted_model <- boost_tree(tree_depth=3, #Determined by random store-item combos
                            trees=500,
                            learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")
boost_wf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)

## Double Loop over all store-item combos
for(s in 1:n.stores){
  for(i in 1:n.items){
    
    ## Subset the data
    train <- item %>%
      filter(store==s, item==i)
    test <- itemTest %>%
      filter(store==s, item==i)
    
    ## Fit the data and forecast
    fitted_wf <- boost_wf %>%
      fit(data=train)
    preds <- predict(fitted_wf, new_data=test) %>%
      bind_cols(test) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    ## Save the results
    if(s==1 && i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
  }
}

vroom_write(x=all_preds, "./submission.csv", delim=",")