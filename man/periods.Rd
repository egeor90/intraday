\name{periods}
\alias{periods}
\title{Change the period of an 'xts' dataset}
\usage{
periods(dataset, k=10, on="minutes", xtsConvert=NULL, output="xts")

}
\description{
Transform the minutely intraday stock price data into different frequencies}
\arguments{

\item{dataset}{an 'ohlcv' dataset, must be 'xts' class. if not, 'xtsConvert = TRUE' can be used for transformation.}
\item{on}{the frequency of time period. must be an integer.}
\item{k}{time period. it can be used with following syntax: "minutes", "hours", "days" and "months"}
\item{xtsConvert}{data transformation into 'xts' class.}
\item{output}{the output class of periodic conversion data. it can be transformed into "xts", data."frame" and "data.table" classes.}
}
\examples{
# DO NOT RUN!
# conversion of 1-min Apple Inc. stock prices into 10-min
aapl <- intraday_live("aapl"),
aapl_10min <- periods(aapl, k=10, on="minutes")

# minutely data to hourly data
aapl <- intraday_live("aapl"),
aapl_10min <- periods(aapl, k=1, on="hours")


# convert from a 'data.table' minutely data into hourly 'xts' data.
csco_dt <- intraday_live(stock="csco", type="data.table")
csco_1h <- periods(csco_dt, k=1, on="hours", xtsConvert = TRUE)
csco_1h_df <- periods(csco_dt, k=1, on="hours", xtsConvert = TRUE, output="data.frame")
}
