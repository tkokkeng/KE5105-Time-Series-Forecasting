KE5105 - Building Electrical Consumption Forecasting
================

Extract, Transform and Load Data 6 - Data Imputation - MD6
==========================================================

Summary of Findings
===================

-   10 periods without any missing data of around 30-day duration is selected from MD6 data
-   Missing data in these periods are simulated assuming the missing data occurs according to an exponential distribution
    -   many missing observations in industrial datasets follow an exponential distribution \[1\]

\[1\] S. Moritz, A. Sardá, T. Bartz-Beielstein, M. Zaefferer, and J. Stork. Comparison of different Methods for Univariate Time Series Imputation in R. ArXiv e-prints, Oct. 2015.

Load libraries
==============

``` r
library(ggplot2)
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(imputeTS)
```

    ## 
    ## Attaching package: 'imputeTS'

    ## The following object is masked from 'package:zoo':
    ## 
    ##     na.locf

Load data
=========

``` r
md6_df <- read.csv("/home/tkokkeng/Documents/KE5105/ETL/source/processed_bldg_data/MD6.csv", header = TRUE, stringsAsFactors = FALSE)
head(md6_df)
```

Convert the Pt\_timeStamp strings to POSIX time
-----------------------------------------------

``` r
md6_df$Pt_timeStamp <- strptime(md6_df$Pt_timeStamp, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
head(md6_df)
```

Look for the largest contiguous subset of non-NA data
-----------------------------------------------------

``` r
PWM_notNA_df <- data.frame(md6_df$PWM_30min_avg)
head(PWM_notNA_df)
```

    ##   md6_df.PWM_30min_avg
    ## 1                   NA
    ## 2                   NA
    ## 3                   NA
    ## 4                   NA
    ## 5                   NA
    ## 6                   NA

### Get a cumulative count of the NAs.

``` r
PWM_notNA_df$na_cumsum = cumsum(is.na(PWM_notNA_df$md6_df.PWM_30min_avg))
head(PWM_notNA_df)
```

    ##   md6_df.PWM_30min_avg na_cumsum
    ## 1                   NA         1
    ## 2                   NA         2
    ## 3                   NA         3
    ## 4                   NA         4
    ## 5                   NA         5
    ## 6                   NA         6

### Remove the rows with NAs, leaving only the data rows. Each consective data row has a unique cumulative count.

``` r
PWM_notNA_df <- PWM_notNA_df[!is.na(PWM_notNA_df$md6_df.PWM_30min_avg),]
head(PWM_notNA_df)
```

    ##     md6_df.PWM_30min_avg na_cumsum
    ## 408                826.5       407
    ## 409                833.0       407
    ## 410                833.0       407
    ## 411                828.5       407
    ## 412                825.5       407
    ## 413                813.0       407

### Group the data rows by their cumulative count and get the frequency which is the size of each contiguous block of data.

``` r
PWM_notNA_df <- as.data.frame(table(PWM_notNA_df$na_cumsum), stringsAsFactors = FALSE)
colnames(PWM_notNA_df) <- c("row", "size")
PWM_notNA_df$row = as.integer(PWM_notNA_df$row)
head(PWM_notNA_df)
```

    ##   row size
    ## 1 407  910
    ## 2 413    1
    ## 3 415    1
    ## 4 423   56
    ## 5 427   92
    ## 6 429  810

### Offset the row numbers to get the correct row index.

``` r
PWM_notNA_df[2:nrow(PWM_notNA_df), c("row")] <- tail(PWM_notNA_df$row, -1) + head(cumsum(PWM_notNA_df$size), -1) + 1
head(PWM_notNA_df)
```

    ##    row size
    ## 1  407  910
    ## 2 1324    1
    ## 3 1327    1
    ## 4 1336   56
    ## 5 1396   92
    ## 6 1490  810

### Find the biggest contiguous data blocks.

``` r
PWM_notNA_df[which(PWM_notNA_df$size == max(PWM_notNA_df$size)),]
```

    ##      row size
    ## 10  4322 1486
    ## 11 19010 1486
    ## 13 21842 1486
    ## 15 24770 1486
    ## 17 27698 1486
    ## 21 32114 1486

### Prepare the data for plotting.

``` r
ts <- xts(md6_df[4322:(4322+1486-1),]$PWM_30min_avg, md6_df[4322:(4322+1486-1),]$Pt_timeStamp)
```

### Plot the time series data for a period without missing data

``` r
autoplot(ts) +
  ylab("Aggregated PWM") +
  xlab("Time") +
  ggtitle("MD6 Aggregated PWM for a Period without Missing Data")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/plot_md6-1.png)

Check that there is no missing data in the selected period.

``` r
which(is.na(md6_df[4322:(4322+1486),]$PWM_30min_avg))
```

    ## [1] 1487

Simulate missing data using an exponential distribution
-------------------------------------------------------

``` r
#' Code of the missing data simulation function
#' @param data - univariate time series
#' @param rate - lambda value for exponential distribution (# events per unit time)
#' @param seed - random seed used for the function

create.missing <- function(data, rate, seed=NULL) {

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
      data[a] <- NA
      tempDelete <- c(tempDelete, a)
    }
  }
  return(list(data=data, na.ind=tempDelete))
}
```

### Exponential Distribution (<https://en.wikipedia.org/wiki/Exponential_distribution>)

![Exponential Distribution](./images/360px-Exponential_pdf.svg.png)

### Simulate missing data for 1 contiguous data block

``` r
ts <- ts(md6_df[4322:(4322+1486-1),]$PWM_30min_avg)
missing <- create.missing(data = ts, rate = .1, seed = 729)
```

### Prepare the simulated data for plotting

``` r
removed_data = md6_df[4322:(4322+1486-1),]$PWM_30min_avg
na_found <- FALSE
temp <- NA
for (i in 1:length(removed_data))
  if (!is.na(missing$data[i])) {
    temp <- removed_data[i]
    if (!na_found) {
      removed_data[i] <- NA
    }
    na_found <- FALSE
  } else if (!na_found) {
    na_found = TRUE
    removed_data[i-1] <- temp
  }
```

### Plot the simulated missing data

``` r
ts_removed_data = ts(removed_data)
ts_data_with_missing = ts(missing$data)
tsm <- cbind(ts_removed_data, ts_data_with_missing)
plot.ts(tsm, plot.type = "single", col = c("red", "blue"), type = "o", pch = 19, cex = .4, ylab = "PWM")
title("MD6 Aggregated PWM with Simulated Missing Data")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/plot_md6_missing-1.png)

``` r
plot.ts(tsm[100:250,], plot.type = "single", col = c("red", "blue"), type = "o", pch = 19, cex = .4, ylab = "PWM")
title("MD6 Aggregated PWM with Simulated Missing Data")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/plot_md6_missing_zoom-1.png)

### Distribution of simulated missing data

``` r
plotNA.distributionBar(missing$data, breaks = 50, main = "MD6 Disribution of NAs")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/distribution_NA-1.png)

### Plot the distribution of the missing data by gap size

``` r
plotNA.gapsize(missing$data, byTotalNA = TRUE, main = "MD6 Occurence of gap sizes (NAs in a row)")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/distribution_NA_gapsize-1.png)

Create the simulated missing data for a set of contiguous data blocks.
----------------------------------------------------------------------

``` r
# Sort the contiguous data blocks by size.
PWM_notNA_df <- PWM_notNA_df[order(-PWM_notNA_df$size),]
head(PWM_notNA_df, 10)
```

    ##      row size
    ## 10  4322 1486
    ## 11 19010 1486
    ## 13 21842 1486
    ## 15 24770 1486
    ## 17 27698 1486
    ## 21 32114 1486
    ## 14 23330 1438
    ## 16 26258 1438
    ## 20 30674 1438
    ## 22 33602 1438

### Save the 10 largest data blocks without missing data to file (.csv)

``` r
for (i in 1:10) {
  write.csv(md6_df[PWM_notNA_df$row[i]:(PWM_notNA_df$row[i]+PWM_notNA_df$size[i]-1),],
            file = paste("md6_no_nan", toString(i), ".csv", sep = ""),
            row.names = FALSE)
}
```

``` r
# Generate missing data for the 10 largest data blocks for a range of lambda values (# events per unit time) .1, .15, .2, .25 

num_datasets = 10  # this is the number of time series data blocks we are extracting from the SDE-3 data to simulate the missing data 
lambda_list = seq(10, 25, by=5)
missing_list = vector("list", length(lambda_list))

# For each lambda value, simulate the missing data.
for (i in 1:length(lambda_list)) {
  
  missing = vector("list", num_datasets)
  
  # For each dataset, simulate the missing data.
  for (j in 1:num_datasets) {
    ts <- ts(md6_df[PWM_notNA_df$row[j]:(PWM_notNA_df$row[j]+PWM_notNA_df$size[j]-1),]$PWM_30min_avg)
    missing[[j]] <- c(create.missing(data = ts, rate = lambda_list[i]/100, seed = 729),
                      row = PWM_notNA_df$row[j], size = PWM_notNA_df$size[j])
  }
  missing_list[[i]] <- c(lambda=lambda_list[i]/100, missing = missing)
}
```

### Save the time series with simulated missing data to file (.csv)

``` r
for (i in missing_list) {
  for (j in 1:num_datasets) {
    data_with_na_df = data.frame(i[[j+1]]$data)
    colnames(data_with_na_df) = c('PWM_30min_avg')
    write.csv(data_with_na_df,
              file = paste("md6_sim_nan_", i$lambda, "_", j, ".csv", sep = ""),
              row.names = FALSE)    
  }
} 
```

``` r
# Check the generated data for 1 block
removed_data = md6_df[missing_list[[1]][[2]]$row:(missing_list[[1]][[2]]$row+missing_list[[1]][[2]]$size-1),]$PWM_30min_avg
na_found <- FALSE
temp <- NA
for (i in 1:length(removed_data)) {
  # Not a nan value
  if (!is.na(missing_list[[1]][[2]]$data[i])) {
    # Remember this value
    temp <- removed_data[i]
    # Not in a nan series of values, change value to nan
    if (!na_found) {
      removed_data[i] <- NA
    }
    # set to in a series of nan values
    na_found <- FALSE
  } else if (!na_found) {
    # 1st nan in series found
    na_found = TRUE
    # The previous value was set to nan, reset the previous value back to the remembered value
    removed_data[i-1] <- temp
  }
}

ts_removed_data = ts(removed_data)
ts_data_with_missing = ts(missing_list[[1]][[2]]$data)
tsm <- cbind(ts_removed_data, ts_data_with_missing)
plot.ts(tsm, plot.type = "single", col = c("red", "blue"), type = "o", pch = 19, cex = .4, ylab = "PWM")
title("MD6 Aggregated PWM with Simulated Missing Data")
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/plot_md6_missing_check-1.png)

``` r
for (i in missing_list) {
  plotNA.gapsize(i[[2]]$data, main=paste("MD6 lambda =", toString(i[[1]])), byTotalNA = TRUE)
  cat("MD6 Missing data statistics for lambda =", toString(i[[1]]), "\n")
  statsNA(i[[2]]$data)
  cat("\n")
}
```

![](ETL6.impute.sim.MD6_files/figure-markdown_github/all_distribution_gapsize-1.png)

    ## MD6 Missing data statistics for lambda = 0.1 
    ## [1] "Length of time series:"
    ## [1] 1486
    ## [1] "-------------------------"
    ## [1] "Number of Missing Values:"
    ## [1] 137
    ## [1] "-------------------------"
    ## [1] "Percentage of Missing Values:"
    ## [1] "9.22%"
    ## [1] "-------------------------"
    ## [1] "Stats for Bins"
    ## [1] "  Bin 1 (372 values from 1 to 372) :      37 NAs (9.95%)"
    ## [1] "  Bin 2 (372 values from 373 to 744) :      29 NAs (7.8%)"
    ## [1] "  Bin 3 (372 values from 745 to 1116) :      40 NAs (10.8%)"
    ## [1] "  Bin 4 (370 values from 1117 to 1486) :      31 NAs (8.38%)"
    ## [1] "-------------------------"
    ## [1] "Longest NA gap (series of consecutive NAs)"
    ## [1] "3 in a row"
    ## [1] "-------------------------"
    ## [1] "Most frequent gap size (series of consecutive NA series)"
    ## [1] "1 NA in a row (occuring 107 times)"
    ## [1] "-------------------------"
    ## [1] "Gap size accounting for most NAs"
    ## [1] "1 NA in a row (occuring 107 times, making up for overall 107 NAs)"
    ## [1] "-------------------------"
    ## [1] "Overview NA series"
    ## [1] "  1 NA in a row: 107 times"
    ## [1] "  2 NA in a row: 12 times"
    ## [1] "  3 NA in a row: 2 times"

![](ETL6.impute.sim.MD6_files/figure-markdown_github/all_distribution_gapsize-2.png)

    ## MD6 Missing data statistics for lambda = 0.15 
    ## [1] "Length of time series:"
    ## [1] 1486
    ## [1] "-------------------------"
    ## [1] "Number of Missing Values:"
    ## [1] 198
    ## [1] "-------------------------"
    ## [1] "Percentage of Missing Values:"
    ## [1] "13.3%"
    ## [1] "-------------------------"
    ## [1] "Stats for Bins"
    ## [1] "  Bin 1 (372 values from 1 to 372) :      55 NAs (14.8%)"
    ## [1] "  Bin 2 (372 values from 373 to 744) :      49 NAs (13.2%)"
    ## [1] "  Bin 3 (372 values from 745 to 1116) :      46 NAs (12.4%)"
    ## [1] "  Bin 4 (370 values from 1117 to 1486) :      48 NAs (13%)"
    ## [1] "-------------------------"
    ## [1] "Longest NA gap (series of consecutive NAs)"
    ## [1] "3 in a row"
    ## [1] "-------------------------"
    ## [1] "Most frequent gap size (series of consecutive NA series)"
    ## [1] "1 NA in a row (occuring 122 times)"
    ## [1] "-------------------------"
    ## [1] "Gap size accounting for most NAs"
    ## [1] "1 NA in a row (occuring 122 times, making up for overall 122 NAs)"
    ## [1] "-------------------------"
    ## [1] "Overview NA series"
    ## [1] "  1 NA in a row: 122 times"
    ## [1] "  2 NA in a row: 32 times"
    ## [1] "  3 NA in a row: 4 times"

![](ETL6.impute.sim.MD6_files/figure-markdown_github/all_distribution_gapsize-3.png)

    ## MD6 Missing data statistics for lambda = 0.2 
    ## [1] "Length of time series:"
    ## [1] 1486
    ## [1] "-------------------------"
    ## [1] "Number of Missing Values:"
    ## [1] 263
    ## [1] "-------------------------"
    ## [1] "Percentage of Missing Values:"
    ## [1] "17.7%"
    ## [1] "-------------------------"
    ## [1] "Stats for Bins"
    ## [1] "  Bin 1 (372 values from 1 to 372) :      65 NAs (17.5%)"
    ## [1] "  Bin 2 (372 values from 373 to 744) :      68 NAs (18.3%)"
    ## [1] "  Bin 3 (372 values from 745 to 1116) :      59 NAs (15.9%)"
    ## [1] "  Bin 4 (370 values from 1117 to 1486) :      71 NAs (19.2%)"
    ## [1] "-------------------------"
    ## [1] "Longest NA gap (series of consecutive NAs)"
    ## [1] "4 in a row"
    ## [1] "-------------------------"
    ## [1] "Most frequent gap size (series of consecutive NA series)"
    ## [1] "1 NA in a row (occuring 161 times)"
    ## [1] "-------------------------"
    ## [1] "Gap size accounting for most NAs"
    ## [1] "1 NA in a row (occuring 161 times, making up for overall 161 NAs)"
    ## [1] "-------------------------"
    ## [1] "Overview NA series"
    ## [1] "  1 NA in a row: 161 times"
    ## [1] "  2 NA in a row: 36 times"
    ## [1] "  3 NA in a row: 6 times"
    ## [1] "  4 NA in a row: 3 times"

![](ETL6.impute.sim.MD6_files/figure-markdown_github/all_distribution_gapsize-4.png)

    ## MD6 Missing data statistics for lambda = 0.25 
    ## [1] "Length of time series:"
    ## [1] 1486
    ## [1] "-------------------------"
    ## [1] "Number of Missing Values:"
    ## [1] 325
    ## [1] "-------------------------"
    ## [1] "Percentage of Missing Values:"
    ## [1] "21.9%"
    ## [1] "-------------------------"
    ## [1] "Stats for Bins"
    ## [1] "  Bin 1 (372 values from 1 to 372) :      80 NAs (21.5%)"
    ## [1] "  Bin 2 (372 values from 373 to 744) :      75 NAs (20.2%)"
    ## [1] "  Bin 3 (372 values from 745 to 1116) :      87 NAs (23.4%)"
    ## [1] "  Bin 4 (370 values from 1117 to 1486) :      83 NAs (22.4%)"
    ## [1] "-------------------------"
    ## [1] "Longest NA gap (series of consecutive NAs)"
    ## [1] "4 in a row"
    ## [1] "-------------------------"
    ## [1] "Most frequent gap size (series of consecutive NA series)"
    ## [1] "1 NA in a row (occuring 176 times)"
    ## [1] "-------------------------"
    ## [1] "Gap size accounting for most NAs"
    ## [1] "1 NA in a row (occuring 176 times, making up for overall 176 NAs)"
    ## [1] "-------------------------"
    ## [1] "Overview NA series"
    ## [1] "  1 NA in a row: 176 times"
    ## [1] "  2 NA in a row: 47 times"
    ## [1] "  3 NA in a row: 13 times"
    ## [1] "  4 NA in a row: 4 times"

### Save the simulated missing data to file.

``` r
saveRDS(missing_list, "sim_missing_data.md6.rds")
```
