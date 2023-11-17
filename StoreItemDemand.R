##
## Store Item Demand
##

library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(timetk) # time series plots
library(embed) # target encoding
library(discrim) # naive bayes
library(ranger) # random forests


### DATA ###
# setwd("C:/Users/bowen/Desktop/Stat348/StoreItemDemand")
store_train <- vroom::vroom("train.csv")
store_test <- vroom::vroom("test.csv")

item_train <- store_train %>%
  filter(store==7, item==11)
item_test <- store_test %>%
  filter(store==7, item==11)


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