## demographics and employment in the united states

##Load the dataset from CPSData.csv into a data frame called CPS, and view the dataset with the summary() and str() commands.

CPS <- read.csv("CPSData.csv")
str(CPS)

## How many interviewees are in the dataset?
nrow(CPS)
## [1] 131302

## Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
v <- CPS$Industry
names(sort(summary(v[!is.na(v)]), decreasing=TRUE)[1])
## [1] "Educational and health services"

## Which state has the fewest interviewees?
names(sort(table(CPS$State), decreasing=FALSE))[1]
## [1] "New Mexico"
## Which state has the largest number of interviewees?
names(sort(table(CPS$State), decreasing=TRUE))[1]
## [1] "California"

## What proportion of interviewees are citizens of the United States?
sum(summary(CPS$Citizenship)[1:2])/nrow(CPS)
## [1] 0.9421943
table(CPS$Race, CPS$Hispanic)
                  
  ##                      0     1
  ## American Indian   1129   304
  ## Asian             6407   113
  ## Black            13292   621
  ## Multiracial       2449   448
  ## Pacific Islander   541    77
  ## White            89190 16731

## Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)

names(which(sapply(CPS, function(x) any(is.na(x)))))

## [1] "MetroAreaCode"    "Married"          "Education"        "EmploymentStatus"
## [5] "Industry"        
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married)) ## YES!


## How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? For this question, treat the District of Columbia as a state (even though it is not technically a state).

tab <- table(is.na(CPS$MetroAreaCode), CPS$State)
length(which(tab[1,]==0))
## [1] 2
length(which(tab[2,]==0))
## [1] 3

## Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?

reg_tab <- table(is.na(CPS$MetroAreaCode), CPS$Region)
props <- sort(reg_tab[2,] / (reg_tab[2,] + reg_tab[1,]))
names(sort(props, decreasing=TRUE)[1])

## Which state has a proportion of interviewees
## living in a non-metropolitan area closest to 30%?
names(sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, function(x) { abs(mean(x) - 0.3) })))[1]

## [1] "Wisconsin"

## Which state has the largest proportion of
## non-metropolitan interviewees, ignoring
## states where all interviewees were non-metropolitan?
metro<- tapply(is.na(CPS$MetroAreaCode), CPS$State, function(x) { mean(x) })

names(sort(metro[which(metro != 1)], decreasing = TRUE))[1]
## [1] "Montana"
