################ Load Environment ##################
# clean workspace
rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse"
)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
}
# 
# # Post-processing for fold 5
# fold5_modifier = function(test_month, shift) {
#   week51.old = test_month %>%
#     filter(Date == ymd('2011-12-23'))
#   week52.old = test_month %>% 
#     filter(Date == ymd('2011-12-30'))
#   
#   merge_modify <- left_join(week51.old, week52.old, by = c('Store', 'Dept')) %>% #same store/depts wk 51 and 52 side by side
#     mutate(Weekly_Pred1.y = Weekly_Pred1.y + Weekly_Pred1.x*shift) %>% #we need to move some from 51 to 52 of predicted vals - see excel
#     mutate(Weekly_Pred1.x = Weekly_Pred1.x*(1-shift)) %>%
#     mutate(Weekly_Pred2.y = Weekly_Pred2.y + Weekly_Pred2.x*shift) %>%
#     mutate(Weekly_Pred2.x = Weekly_Pred2.x*(1-shift)) %>%
#     mutate(Weekly_Pred3.y = Weekly_Pred3.y + Weekly_Pred3.x*shift) %>%
#     mutate(Weekly_Pred3.x = Weekly_Pred3.x*(1-shift))
#   
#   week51.new <- merge_modify %>%
#     select(c('Store', 'Dept'), ends_with('.x'))  %>%
#     rename(Date = Date.x, IsHoliday = IsHoliday.x, Weekly_Pred1 = Weekly_Pred1.x, Weekly_Pred2 = Weekly_Pred2.x, Weekly_Pred3 = Weekly_Pred3.x)
#   
#   week52.new <- merged %>% 
#     select(c('Store', 'Dept'), ends_with('.y')) %>%
#     rename(Date = Date.y, IsHoliday = IsHoliday.y, Weekly_Pred1 = Weekly_Pred1.y, Weekly_Pred2 = Weekly_Pred2.y, Weekly_Pred3 = Weekly_Pred3.y)
#   
#   binded = bind_rows(week51.new, week52.new)
#   result <- test_month %>% 
#     left_join(binded, by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
#     mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
#     mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
#     mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
#     select(-Weekly_Pred1.x, -Weekly_Pred1.y,
#            -Weekly_Pred2.x, -Weekly_Pred2.y,
#            -Weekly_Pred3.x, -Weekly_Pred3.y)
#   result
# }


# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {  #test_month is the sub data frame we want to upate
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date')) #merge to the test_month the predicted values based on store/date
  #note: all of this is happening for 1 dept within the loop
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  if (num_model == 1) {
    test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  } else if(num_model == 2) {
    test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  } else {
    test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  }
  
  test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
  # if (t == 5) {
  #   test_month = fold5_modifier(test_month, 0.05)
  # }
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}


##### Model Building Functions #####
###
#model 1
# Forecasts out the last observation in the training data
naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  num_comp = 12
  svd_out = svd(train_ts[, 2:ncol(train_ts)], nu=num_comp, nv=num_comp)
  s = diag(svd_out$d[1:num_comp])
  train_ts[, 2:ncol(train_ts)] <- svd_out$u %*% s %*% t(svd_out$v)
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){ #first column has dates
    store_ts <- ts(train_ts[, j], frequency=52) #making the column for a store a time series
    test_ts[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_ts
}

###
# Model 2 -- Time Series Linear Regression
tslr_model = function(train_ts, test_ts) {
  num_forecasts = nrow(test_ts)
  train_ts[is.na(train_ts)] = 0
  num_comp = 12
  svd_out = svd(train_ts[, 2:ncol(train_ts)], nu=num_comp, nv=num_comp)
  s = diag(svd_out$d[1:num_comp])
  train_ts[, 2:ncol(train_ts)] <- svd_out$u %*% s %*% t(svd_out$v)
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts = ts(train_ts[, j], frequency=52)
    model = tslm(store_ts ~ trend + season)
    output = forecast(model, h=num_forecasts)
    test_ts[, j] <- as.numeric(output$mean)
  }
  test_ts
}

###
#model 3 - SVD/stlf-arima
svd_model = function(train_ts, test_ts){
  num_forecasts = nrow(test_ts)
  train_ts[is.na(train_ts)] = 0
  num_comp = 12
  svd_out = svd(train_ts[, 2:ncol(train_ts)], nu=num_comp, nv=num_comp)
  s = diag(svd_out$d[1:num_comp])
  train_ts[, 2:ncol(train_ts)] <- svd_out$u %*% s %*% t(svd_out$v) #transpose #taking only the first 12 eigen values and multiplying
  
  #forecast
  for(j in 2:ncol(train_ts)){
    store_ts = ts(train_ts[, j], frequency=52)
    output = stlf(store_ts, h=num_forecasts, s.window=3, method='arima', ic='bic')
    test_ts[, j] <- as.numeric(output$mean)
  }
  test_ts
  
}



##### Prediction Loop #####

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_test) #this adds the previous fold for predicting the next fold etc
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1)) #defining period for test phase
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date) #taking rows of test dataframe that we want to input values for
  
  # Dates are not the same across months! ????
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either ?????
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions ????
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores) ????????????????????????????
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    # filter for the particular department in the training data
    train_dept_ts <- train %>% #take all training data for one dept - across stores
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>% #creating Y matrix after merging, may have missing values
      spread(Store, Weekly_Sales)
    #head(train_dept_ts)
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    ###### Model Fitting / Forecasting ######
    
    # MODEL 1 - naive forecast
    f_naive <- naive_model(train_dept_ts, test_dept_ts) #returns predicted time series for current dept for all stores
    test_month <- update_forecast(test_month, f_naive, dept, 1) #updates predicted values in initial test dataframe
    #write.csv(train_dept_ts, 'mat.csv',row.names = FALSE)
    #break  
    
    #MODEL 2 - time series linear regression
    tslr_pred = tslr_model(train_dept_ts, test_dept_ts)
    test_month = update_forecast(test_month, tslr_pred, dept, 2) 
    
    #MODEL 3
    if(t>7){
      stlf_svd_pred = svd_model(train_dept_ts,test_dept_ts)
      test_month = update_forecast(test_month, stlf_svd_pred, dept, 3)  
    } else {
      test_month = update_forecast(test_month, tslr_pred, dept, 3) 
    }

          
    }
  
  # update global test dataframe
  #if(t==5){
  #  write.csv(test_month,'fold5_pred.csv',row.names = FALSE)
  #}
  update_test(test_month) #note the test month just contains 2 months but for all stores and departments. At every iteration
                          #the predicted values for the 2 month period is updated for a particular dept. After all departments
                          #are calculated the global test dataframe is updated for those two months for all stores/depts
}


#mat = read.csv('mat.csv')
#z <- svd(mat[, 2:ncol(mat)])
#z1 = prcomp(mat[, 2:ncol(mat)], scale. = TRUE)
#screeplot(z1)


#train_data = read.csv('mat.csv')
#lines(as.character(train_data[,1]),train_data[,2])
#train_data$Date <- as.Date(train_data$Date, "%Y-%m-%d")
#plot(X1 ~ Date, train_data, xaxt = "n", type = "l")
#axis(1, train_data$Date, format(train_data$Date, "%b %d"), cex.axis = .7)

