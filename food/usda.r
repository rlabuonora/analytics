usda <- read.csv("USDA.csv")
str(usda)
summary(usda)

## hay un alimento con mucho sodio, cual es?
usda$Description[which.max(usda$Sodium)]
names(usda)
high_sodium <- subset(usda, Sodium>10000)
high_sodium$Description

## cuanta sal tiene el caviar
usda$Sodium[match("CAVIAR", usda$Description)]

## para hacernos una idea de cuanto es mucho sodio
summary(usda$Sodium)
mean(usda$Sodium, na.rm=TRUE)
sd(usda$Sodium, na.rm=TRUE)
plot(usda$Protein, usda$TotalFat, xlab="Vitamin", ylab="", main="")
hist(usda$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels", xlim=c(0, 100), breaks=1000)
boxplot(usda$Sugar)
