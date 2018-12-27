# Load libraries
library(imputeTS)

## Simulate missing data using an exponential distribution
#' Code of the missing data simulation function
#' @param data - univariate time series
#' @param rate - lambda value for exponential distribution (# events per unit time)
#' @param seed - random seed used for the function

create.missing <- function(data, rate, gapsize, seed=NULL) {
  
  ## Only for time series
  if (class(data) != "ts") {
    stop("Provided data is not a time series.")
  }
  
  ## No missing data (pass-through)
  if (rate == 0) {
    return(data)
  }
  
  ## Save original parameters
  t <- time(data)
  f <- frequency(data)
  
  ##Setting of random seed
  if (!is.null(seed))
    set.seed(seed)
  
  ## Initialize index
  a <- 0
  
  ## Indices of removed entries
  tempDelete <- numeric(0)
  while (a < length(data)) {
    ## 'ceiling' is to avoid possible zeros
    a <- ceiling(a + rexp(1, rate))
    if ( a <= length(data) ) {
      ## Set all data from a to a+gapsize to NA
      for (i in 0:(gapsize-1)) {
        a <- a + 1
        if ( a <= length(data) ) {
          data[a] <- NA
          tempDelete <- c(tempDelete, a)
        } else {
          break
        }
      }
    }
  }
  return(list(data=data, na.ind=tempDelete))
}

## Prepare simulated missing data for plotting.
prepare.plot.data <- function(missing, orig_data) {
  na_found <- FALSE
  temp <- NA
  for (i in 1:length(orig_data))
    if (!is.na(missing$data[i])) {
      temp <- orig_data[i]
      if (!na_found) {
        orig_data[i] <- NA
      }
      na_found <- FALSE
    } else if (!na_found) {
      na_found = TRUE
      orig_data[i-1] <- temp
    }
  # This is the original data minus the simulated NAs. Samples before and after
  # each NA are also changed to NA for plotting purposes.
  return(orig_data)
}

## Generate missing data for the 10 largest data blocks for a range of lambda values (# events per unit time) .1, .15, .2, .25 
create.all.missing <- function(lambda_list, gap_size_list, PWM_notNA_df, PWM_df) {
  
  # Initialise the list of simulated missing data
  missing_list = vector("list", length(lambda_list) * length(gap_size_list))
  
  # For each lambda value
  for (i in 1:length(lambda_list)) {
    
    # For each gap size, 
    for (j in 1:length(gap_size_list)) {
      
      # List of time series data with simulated missing data for 1 value of lambda and
      # 1 value of gap size 
      #missing = vector("list", num_datasets)
      missing = vector("list", nrow(PWM_notNA_df))
      
      # For each dataset, simulate the missing data.
      for (k in 1:nrow(PWM_notNA_df)) {
        
        ts <- ts(PWM_df[PWM_notNA_df$row[k]:(PWM_notNA_df$row[k]+PWM_notNA_df$size[k]-1),]$PWM_30min_avg)
        missing[[k]] <- c(create.missing(data = ts, rate = lambda_list[i]/100, gap_size_list[j], seed = 729),
                          row = PWM_notNA_df$row[k],
                          size = PWM_notNA_df$size[k])
      }
      # Add to list of simulated missing data
      missing_list[[length(gap_size_list) * (i - 1) + j]] <- c(lambda=lambda_list[i]/100,
                                                               missing = missing, gapsize = gap_size_list[j])
    }
  }
  return(missing_list)
}

## Perform data imputation and evaluate the accuracy using RMSE.
## For data with **variable** gap size, varGapSize = TRUE.
## For data with specified **constant** gap size, varGapSize = FALSE.
computeResults <- function(bldg_name, missing_list, data_df, varGapSize = TRUE) {
    
  # Create folder for storing imputed data.
  if (!dir.exists(file.path(getwd(), "data"))) {
    dir.create(file.path(getwd(), "data"))
  }
  
  if (varGapSize) {
    num_datasets = length(missing_list[[1]]) - 1  # each list has lambda, missing1, ..., missing<n>
  } else {
    num_datasets = length(missing_list[[1]]) - 2  # each list has lambda, missing1, ..., missing<n>, gapsize
  }
  num_config = length(missing_list)  # this is the number of different lambdas or combinations of lambdas & gapsizes
  results = vector('list', num_config * num_datasets)
  idx = 1
  
  for (i in missing_list) {
    
    for (j in 1:num_datasets) {
      # test data
      data_with_na = i[[j+1]]$data
      data_true = data_df[i[[j+1]]$row:(i[[j+1]]$row+i[[j+1]]$size-1), ]$PWM_30min_avg  # all original PWM values (no NAs)
      na_true = data_true[i[[j+1]]$na.ind]  # the original PWM values which are simulated NAs
      
      # Impute the missing values using structural model and Kalman smoothing
      imp_struct <- na.kalman(data_with_na)
      # Impute the missing values using ARIMA model and Kalman smoothing
      imp_arima <- na.kalman(data_with_na, model = "auto.arima")
      # Impute the missing values using spline interpolation
      imp_spline <- na.interpolation(data_with_na, option = "spline")
      # Impute the missing values using moving average
      imp_ma <- na.ma(data_with_na, k=4, weighting="exponential")  
      
      # Save the imputed data to file (.csv)
      for (k in list("imp_struct", "imp_arima", "imp_spline", "imp_ma")) {
        df = data.frame(eval(parse(text = k)))
        colnames(df) = c('PWM_30min_avg')
        df$Pt_timeStamp <- data_df[i[[j+1]]$row:(i[[j+1]]$row+i[[j+1]]$size-1), ]$Pt_timeStamp
        df$imputed <- is.na(data_with_na)
        df <- df[, c("Pt_timeStamp", "imputed", "PWM_30min_avg")]
        write.csv(df,
                  file = file.path(getwd(), "data", paste(bldg_name, "_", k, "_", i$lambda, "_", j, ".csv", sep = "")),
                  row.names = FALSE)
      }
      
      # Extract all the imputed na values
      na_imp_struct = imp_struct[i[[j+1]]$na.ind]
      na_imp_arima = imp_arima[i[[j+1]]$na.ind]
      na_imp_spline = imp_spline[i[[j+1]]$na.ind]
      na_imp_ma = imp_ma[i[[j+1]]$na.ind]
      
      # compute rmse
      rmse_struct = sqrt(mean((na_imp_struct - na_true)^2))
      rmse_arima = sqrt(mean((na_imp_arima - na_true)^2))
      rmse_spline = sqrt(mean((na_imp_spline - na_true)^2))
      rmse_ma = sqrt(mean((na_imp_ma - na_true)^2))
      
      #Update results
      if (varGapSize) {
        results[[idx]] <- c(i$lambda, rmse_struct, rmse_arima, rmse_spline, rmse_ma)
      } else {
        results[[idx]] <- c(i$lambda,
                            i$gapsize, length(i[[j+1]]$na.ind) / length(i[[j+1]]$data),
                            rmse_struct, rmse_arima, rmse_spline, rmse_ma)
      }
      idx = idx + 1
    }
  }
  return(results)
}

## Perform data imputation for missing data up to the maximum specified gap size.
## Missing data of gap size larger than the maximum are ignored.
imputeData <- function(data, maxGapSize = 3) {
  #imp_train <- na.kalman(train$PWM_30min_avg)
  imp <- na.kalman(data)
  # remove imputed data for gaps larger than max gap
  big_gaps_df <- findGapsBiggerThan(data, maxGapSize = 3)
  if (nrow(big_gaps_df) > 0) {
    for (i in 1:nrow(big_gaps_df)) {
      imp[big_gaps_df[i, "row"]:(big_gaps_df[i, "row"] + big_gaps_df[i, "size"] - 1)] <- array(NA, dim = big_gaps_df[i, "size"])
    }
  }
  return(imp)
}

## Find all missing data of gap size larger than the specified maximum gap size.
## Each missing data gap is identified by row and gap size.
findGapsBiggerThan <- function(data, maxGapSize = 3) {
  
  # Find all gaps in the data
  na_df <- data.frame(data)
  na_df$notna_cumsum <- cumsum(!is.na(data))
  na_df <- na_df[is.na(data), ]
  na_df <- as.data.frame(table(na_df$notna_cumsum), stringsAsFactors = FALSE)
  colnames(na_df) <- c("row", "size")
  na_df$row <- as.integer(na_df$row)
  
  na_df[1, c("row")] <- na_df[1, c("row")] + 1

  na_df[2:nrow(na_df), c("row")] <- tail(na_df$row, -1) + head(cumsum(na_df$size), -1) + 1
  # Remove the gaps less than equal to max gap size 
  na_df <- na_df[na_df$size > maxGapSize,]
  
  return(na_df)
}