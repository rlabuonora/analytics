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

# Using the plot function, plot the Date on the x-axis 
# and the StockPrice on the y-axis, for Coca-Cola.

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

# Around what year did Coca-Cola has its highest stock price in this time period?
# 1973

# Around what year did Coca-Cola has its lowest stock price in this time period?
# 1980

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

# In March of 2000, the technology bubble burst, and a stock market crash occurred. 
# According to this plot, which company's stock dropped more?
# ProcterGamble

# Around 1983, the stock for one of these companies 
# (Coca-Cola or Procter and Gamble) was going up, 
# while the other was going down. Which one was going up?

abline(v=as.Date(c("1983-06-06")), lwd=2)
# Procter and Gamble

# In the time period shown in the plot, 
# which stock generally has lower values?
# Coca-Cola 


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="black")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="magenta")

# Which stock fell the most 
# right after the technology bubble burst in March 2000?
# General Electric (GE) (blue)

# Which stock reaches the highest value in the time period 1995-2005?
# IBM


# In October of 1997, there was a global stock market crash 
# that was caused by an economic crisis in Asia. 
# Comparing September 1997 to November 1997, 
# which companies saw a decreasing trend in their stock price?

abline(v=as.Date(c("1997-08-30")), lwd=1)

# ProcterGamble (Magenta)
# Boeing (Green)

# In the last two years of this time period (2004 and 2005) 
# which stock seems to be performing the best, 
# in terms of increasing stock price?
# Boeing


# Lastly, let's see if stocks tend to be higher or lower during certain months. 
# Use the tapply command to calculate the mean stock price of IBM, sorted by months. To sort by months, use
# For IBM, compare the monthly averages to the overall average stock price. 
# In which months has IBM historically had a higher stock price (on average)? 

which(tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice))
# April February  January    March      May 
# 1        4        5        8        9 



which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))

tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
# December
