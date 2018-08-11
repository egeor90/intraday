\name{intraday}
\alias{intraday}
\title{Retrieve minutely stock price data in last 60 days}
\usage{
intraday(stock, days, type="xts")

}
\description{
Download minutely stock prices days in "xts", "data.frame" or "data.table" format. This process is available for max. last 60 days.
Time of the dataset is formatted in US Eastern timezone.
}
\arguments{

\item{stock}{the ticker name of the stock. you are free to write the ticker in lower or uppercase characters.}
\item{days}{the number of last n days.}
\item{type}{type of dataset output. default format is "xts". it can also have an output with "data.frame" or "data.table" formats.}

}
\examples{
# DON'T RUN!
# 'xts' output for Apple Inc. minutely stock prices within last 10 days
intra_xts <- intraday(stock="AAPL", days=10, type="xts")

# 'data.frame' output for Pfizer Inc. minutely stock prices within last 20 days
intra_df <- intraday(stock="pfe", days=20, type="data.frame")

# 'data.table' output for Cisco Inc. minutely stock prices within last 30 days
intra_dt <- intraday(stock="CsCo", days=30, type="data.table")

}
