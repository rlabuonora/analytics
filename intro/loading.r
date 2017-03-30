## csvs
who <- read.csv("WHO.csv")

## str
str(who)

## resumen
summary(who)

## subset
who_euro <- subset(who, Region=="Europe")
write.csv(who_euro, file="euro.csv")

