# Download and read the files into R
library(readr)


# IBM <- read_csv("IBMStock.csv")
# GE <- read_csv("GEStock.csv")
# ProcterGamble <- read_csv("ProcterGambleStock.csv")
# CocaCola <- read_csv("CocaColaStock.csv")
# Boeing <- read_csv("BoeingStock.csv")
# 
# 
# IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
# GE$Date <- as.Date(GE$Date, "%m/%d/%y")
# ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
# CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
# Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

abrir <- function(empresa) {
  df <- read_csv(paste(empresa, "Stock.csv", collapse="", sep=""))
  df$Date <- as.Date(df$Date, "%m/%d/%y")
  df
}

IBM <- abrir("IBM")
GE <- abrir("GE")
ProcterGamble<- abrir("ProcterGamble")
CocaCola <- abrir("CocaCola")
Boeing <- abrir("Boeing")

# How many observations are there in each data set?
nrow(IBM)
# 480

# What is the earliest year in our datasets?
min(IBM$Date)
# "1970-01-01"

# What is the latest year in our datasets?
max(IBM$Date)
# "2009-12-01"

# What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)
# 144.375

# What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)
# 9.293636

# What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)
# 146.5843

# What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)
#  44.8834


# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)
# 18.19414

# Using the plot function, plot the Date on the x-axis and 
# the StockPrice on the y-axis, for Coca-Cola.