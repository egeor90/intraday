\name{intraday_live}
\alias{intraday_live}
\title{Retrieve live stock price data for current or last working day}
\usage{
intraday_live(stock, type="xts")

}
\description{
Download live stock prices for current (or if market does not work, for last working day) day in "xts", "data.frame" or "data.table" format. The timezone of dataset is formatted in US Eastern timezone.
}
\arguments{

\item{stock}{the ticker name of the stock. you are free to write the ticker in lower or uppercase characters.}
\item{type}{type of dataset output. default format is "xts". it can also have an output with "data.frame" or "data.table" formats.}

}
\examples{
# DON'T RUN!
# xts output for Apple Inc. minutely live stock prices
live_xts <- intraday("AAPL", type="xts")

# data.frame output for Pfizer Inc. minutely live stock prices
live_df <- intraday("Pfe", type="data.frame")

# data.table output for Cisco Inc. minutely live stock prices
live_dt <- intraday_live(stock="csco", type="data.table")

}
