##
## Store Item Demand
##

library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(timetk) # time series plots
library(embed) # target encoding


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
nStores <- max(store_train$store)
nItems <- max(store_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- store_train %>%
    filter(store==s, item==i)
    storeItemTest <- store_test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
    
    ## Predict storeItem sales
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}
