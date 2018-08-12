library(tidyverse)
library(chron)
library(jsonlite)
library(xts)
library(data.table)
library(rjson)

intraday <-  function(stock,days,type="xts"){
  options(warn = -1)
  if(days > 60){
    stop("Days should be less than 60 days!")
  }
  if(all(type != c("xts","data.frame","data.table"))){
    stop("Please enter a valid type: 'xts', 'data.frame' or 'data.table'")
  }
  stock <- tolower(stock)
  dates <- NA

  for(i in days:0){
    if(!is.weekend(as.Date(Sys.Date()-i))){
      dates[i+1] <- format(as.Date(Sys.Date()-i), "%Y%m%d")
    }
  }
  dates <- rev(na.omit(dates))

  url <- paste0("https://api.iextrading.com/1.0/stock/",stock,"/chart/date/",dates)

  for(i in 1:length(url)){
    data_ <- fromJSON(paste(readLines(url[i]), collapse=""))
    data_[,1:ncol(data_)] <- apply(data_[,1:ncol(data_)], 2, function(x) as.vector(unlist(x)))
    data_ <- data_[colnames(data_) %in% c("date","minute","open","marketHigh","marketLow","close","volume")]

    data_ <- data_ %>%
      mutate(date_time = as.POSIXct(paste(data_$date," ", data_$minute), format = "%Y%m%d %H:%M", tz = "America/New_york")) %>%
      select(date_time,open,high=marketHigh,low=marketLow,close,volume)

    if(!exists("stock_data")){
      stock_data <- data_
    }else{
      stock_data <- rbind(stock_data,data_)
    }
  }
  stock_data[,-1] <- apply(stock_data[,-1], 2, function(x) as.numeric(x))
  stock_data <- if(type == "xts"){
    xts(stock_data[,-1],stock_data[,1])
  }else if(type == "data.frame"){
    stock_data
  }else if(type == "data.table"){
    data.table(stock_data)
  }
  return(stock_data)
}

intraday_live <- function(stock,type="xts"){
  options(warn = -1)
  if(all(type != c("xts","data.frame","data.table"))){
    stop("Please enter a valid type: 'xts', 'data.frame' or 'data.table'")
  }
  stock <- tolower(stock)

  url <- paste0("https://api.iextrading.com/1.0/stock/",stock,"/chart/1d")
  data_ <- rjson::fromJSON(paste(readLines(url), collapse=""))

  data_ <- data.frame(Reduce(rbind, data_))

  if(any(colnames(data_) %in% c("open","close"))){
    data_ <- data_[colnames(data_) %in% c("date","minute","open","marketHigh","marketLow","close","volume")]
    data_[,1:ncol(data_)] <- apply(data_[,1:ncol(data_)], 2, function(x) as.vector(unlist(x)))

    stock_data <- data_ %>%
      mutate(date_time = as.POSIXct(paste(data_$date," ", data_$minute), format = "%Y%m%d %H:%M", tz = "America/New_york")) %>%
      select(date_time,open,high=marketHigh,low=marketLow,close,volume)
  }else{
    data_ <- data_[colnames(data_) %in% c("date","minute","marketOpen","marketHigh","marketLow","marketClose","volume")]
    data_[,1:ncol(data_)] <- apply(data_[,1:ncol(data_)], 2, function(x) as.vector(unlist(x)))
    
    stock_data <- data_ %>%
      mutate(date_time = as.POSIXct(paste(data_$date," ", data_$minute), format = "%Y%m%d %H:%M", tz = "America/New_york")) %>%
      select(date_time,open=marketOpen,high=marketHigh,low=marketLow,close=marketClose,volume)
  }

  stock_data[,-1] <- apply(stock_data[,-1], 2, function(x) as.numeric(x))
  stock_data <- if(type == "xts"){
    library(xts)
    xts(stock_data[,-1],stock_data[,1])
  }else if(type == "data.frame"){
    stock_data
  }else if(type == "data.table"){
    data.table(stock_data)
  }
  return(stock_data)
}

periods <- function(dataset, k=10, on="minutes", xtsConvert = NULL, output="xts"){
  if(all(output != c("xts","data.table","data.frame"))){
    stop("Output must be one of the following classes: 'xts', 'data.frame' or 'data.table'")
  }
  if(class(dataset)[1]!="xts" & !isTRUE(xtsConvert)){
    stop("Dataset must be converted into 'xts': try xtsConvert = TRUE")
  }else if(isTRUE(xtsConvert)){
    if(class(dataset)[1] == "data.table"){
      dataset <- as.data.frame(dataset)
      dataset <- xts(dataset[,-1], as.POSIXct(dataset[,1], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }else if(class(dataset)[1] == "xts"){
      dataset <- dataset
    }else if(class(dataset)[1] == "data.frame"){
      dataset <- xts(dataset[,-1], as.POSIXct(dataset[,1], format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
    }
  }
    my.endpoints <- endpoints(dataset,k=k,on=on)
    my.endpoints[2:(length(my.endpoints)-1)] <- my.endpoints[2:(length(my.endpoints)-1)]+1

    open_ <- period.apply(dataset[,"open"],
                          INDEX = my.endpoints,
                          FUN = function(x) head(x, 1))
    high_ <- period.apply(dataset[,"high"],
                          INDEX = my.endpoints,
                          FUN = function(x) max(x, na.rm = T))
    low_ <- period.apply(dataset[,"low"],
                         INDEX = my.endpoints,
                         FUN = function(x) min(x, na.rm = T))
    close_ <- period.apply(dataset[,"close"],
                           INDEX = my.endpoints,
                           FUN = function(x) tail(x, 1))
    volume_ <- period.apply(dataset[,"volume"],
                            INDEX = my.endpoints,
                            FUN = function(x) sum(x, na.rm = T))
    data_period <- merge.xts(open_,high_,low_,close_,volume_)

    if(output == "xts"){
      data_period <- data_period
    }else if(output == "data.frame"){
      data_period <- as.data.frame(data_period)
      data_period <- data_period %>%
        mutate(date_time=as.POSIXct(rownames(data_period), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
        select(date_time,open,high,low,close,volume)
    }else if(output == "data.table"){
        data_period <- as.data.table(data_period)
        colnames(data_period)[1] <- "date_time"
    }
return(data_period)
}
