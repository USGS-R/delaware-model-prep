# Functions to calculate metrics for model evaluation.
library(docstring) ## needed to call the function that are not in a package.
# Mean Absolute Error (MAE) metric
calc_mae <- function(observe_data, predict_data, n_digits = 2) {
  #' Mean Absolute Error (MAE)
  #' @description  Calculates MAE metric to compare the observed data to the modeled data.
  #' @details Finds the mean of the absolute value error (difference between the observed and modeled data), then rounds to the users-specified decimal places. 
  #' @param observe_data the observed data column. 
  #' @param predict_data the modeled predicted data column.
  #' @param n_digits the number of decimals of places to round the calculated MAE value. The function rounds to 2 decimal places by default. 
  #' @return A dataframe or table same class as input data.
  mae = round((mean(abs(observe_data - predict_data), na.rm = TRUE )), digits = n_digits)
  return(mae)
}

# Root Mean Square Error (RMSE)
calc_rmse <- function(observe_data, predict_data, n_digits = 2 ){
  #' Root Mean Square Error (RMSE)
  #' @description Calculates the RMSE metric to compare the observed data and modeled data.
  #' @details Finds the square root of the squared error (difference between the observed and modeled data), then rounds to the users-specified decimal places. 
  #' @param observe_data the observed data column. 
  #' @param predict_data the modeled predicted data column.
  #' @param n_digits the number of decimals of places to round the calculated RMSE value. The function rounds to 2 decimal places by default.
  #' @return A dataframe or table same class as input data.
  rmse <- round(sqrt(mean((observe_data - predict_data) ^2, na.rm = TRUE )), digits = n_digits)
  return(rmse)
}

# Mean Absolute Relative Error (MARE)  
calc_mare <- function(observe_data, predict_data, n_digits = 2){
  #' Mean Absolute Relative Error (MARE)
  #' @description Calculates MARE by finding the ratio of the overall agreement between the observed data and modeled data.
  #' @details Finds the mean of the ratio between the absolute value of the error (the difference between observed and predicted data) and the observed data. Then, rounds to the users-specified decimal places. It is often expressed in percentage terms. 
  #' @param observe_data the observed data column. 
  #' @param predict_data the modeled predicted data column.
  #' @param n_digits the number of decimals of places to round the calculated mare value. The function rounds to 2 decimal places by default. 
  #' @return A dataframe or table same class as input data.
  observe_data = ifelse( observe_data %in% 0, 0.1, observe_data)
  mare <- round(mean((abs(observe_data - predict_data) / observe_data), na.rm = TRUE), digits = n_digits)
  return(mare)
} 

# Max data-values and Time of max. 
calc_tim_temp_max <- function(data_in, observe_col, predict_col, date_col,  date_range = 170:245, n_digits = 2) {
  #' Timing and Magnitude of Maximums
  #' @description Calculates the error between the maximum observed and modeled data values. And it calculates the occurrence error (observed occurrence and predicted occurrence) that is associated with the found maximum data values. 
  #' @details Finds the maximum observed and modeled data values. While providing the day of the year associated with the occurrence of maximum data values. Finally, this function will calculate the error (the difference between observed and predicted data) between the maximum values, then rounds to the users-specified decimal places. And the error for the occurrence (day of the year) of the maximum values.
  #' @param data_in A dataframe or table.  
  #' @param observe_col the observed data column. 
  #' @param predict_col the modeled predicted data column.
  #' @param date_col vector of date class
  #' @param date_range the start day and end day of the year. The date_range target is set to start on 170 day and end on the 245 day of the year by default. The user can specified a date range if desired.
  #' @param n_digits the number of decimal of places to round the calculated mare value. The function rounds to 2 decimal places by default. 
  #' @return This function returns a dataframe that contains the maximum data-values, timing associated with the maximum data-values, the maximum values error, and the timing error for the observed and predicted.
  max_fun_out <- data_in %>%
    summarize(max_obs = max({{observe_col}}),
              max_timing_obs = lubridate::yday(date[which.max({{observe_col}})]),
              max_mod = max({{predict_col}}),
              max_timing_mod = lubridate::yday(date[which.max({{predict_col}})]),
              summer_complete = all(date_range %in% lubridate::yday(date))) %>%
    filter(summer_complete) %>%
    mutate(error_obs_pred = round(abs(max_obs - max_mod), digits = n_digits),
           error_max_timing = abs(max_timing_obs - max_timing_mod)) 
  return(max_fun_out)
}

## Nash coefficient of efficiency (Nash):
calc_nash <- function(observe_col, predict_col, n_digits = 2){
  #' Coefficient of Efficiency Nash
  #' @description Calculates the overall level of agreement between the observed and modeled data. 
  #' @details Calculated the Nash-Sutcliffe Efficiency metric for model evaluation. This metric typically ranges between 0 to 1: a measurement of 1 indicates a perfect model, a measurement of 0 indicates that the model is not better than  a one parameter model "no knowledge", and negative values are permitted and indicate that the model is preforming worse than a "no knowledge" model. The function records 1 minus the ratio of the sum of error (the difference between observed and predicted data) and the sum of observed data deviation from the mean of the observed data. Then, rounds to the users-specified decimal places. 
  #' @param observe_col the observed data column. 
  #' @param predict_col the modeled predicted data column.
  #' @param n_digits the number of decimal of places to round the calculated mare value. The function rounds to 2 decimal places by default. 
  #' @return A dataframe or table same class as input data.
  nash = round(1 - ((sum((observe_col - predict_col) ^ 2)) / (sum((observe_col - mean(observe_col)) ^2))), digits = n_digits)  
}

## Exceedance metric
calc_exceedance <- function(observe_col, predict_col, metric, threshold = 25.5){
  #' Threshold Exceedance Metric
  #' @description Calculates the overall level of agreement between observed and predicted exceedances of the target threshold.
  #' @details The function calculates the proportion of model predictions that meet the metric criteria related to exceedance of a defined target "temperature" threshold. 
  #' @param observe_col the observed data column. 
  #' @param predict_col the modeled predicted data column.
  #' @param metric the target metric: proportion true positive ("prop_true_pos"), proportion true negative ("prop_true_neg"), proportion false-negative ("prop_false_neg"), and proportion false-positive ("prop_false_pos").
  #' @param threshold the target threshold is set to 25.5 by default. The user is able to adjust the threshold to suit the desired model evaluation.
  #' @return  If metric = "prop_true_pos", returns a value equal to the proportion of predictions where both the prediction and observation are above the target threshold. If metric = "prop_true_neg", returns a value equal to the proportion of predictions where both the prediction and observation are below the target threshold. If metric = "prop_false_neg", returns a value equal to the proportion of predictions where the observation is above the threshold, but the prediction is below the threshold. And, if the metric = "prop_false_pos", returns a value equal to the proportion of prediction where the observation is below the threshold and the prediction is above the threshold.
  observe_exceeds <- observe_col > threshold
  predict_exceeds <- predict_col > threshold
  
  if (metric == 'prop_false_neg') {
    return(round(sum(observe_exceeds == TRUE & predict_exceeds == FALSE) / n(), 2))
  } else if (metric == 'prop_false_pos') {
    return(round(sum(observe_exceeds == FALSE & predict_exceeds == TRUE) / n(), 2))
  }
  else if (metric == 'prop_true_pos'){
    return(round(sum(observe_exceeds == TRUE & predict_exceeds == TRUE) / n(), 2))
  }
  else if (metric == 'prop_true_neg') {
    return(round(sum(observe_exceeds == FALSE & predict_exceeds == FALSE) / n(), 2))
  }
}
