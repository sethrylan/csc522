library(RcmdrPlugin.IPSUR)
data(RcmdrTestDrive)
attach(RcmdrTestDrive)
names(RcmdrTestDrive)

#3.1
summary(RcmdrTestDrive)

#3.2
.Table <- table(RcmdrTestDrive$race)
.Table  # counts for race
round(100*.Table/sum(.Table), 2)  # percentages for race
remove(.Table)

#3.3
tapply(RcmdrTestDrive$salary, list(gender=RcmdrTestDrive$gender), mean,
  na.rm=TRUE)

#3.4
x <- sort(reduction)
x[137]
IQR(x)
fivenum(x) # aka, summary(x)
fivenum(x)[4] - fivenum(x)[2]  # close to IQR(x)

#3.5
library(e1071)
c(mean(before), median(before))
c(mean(after), median(after))

limits <- range(before, after)
par(mfrow = c(1, 2))
boxplot(before, ylim=limits)
boxplot(after, ylim=limits)

sd(before)       # 'before' is symmetric :: use std deviation
mad(after)       # 'after' is left-skewed :: use median absolute deviation
IQR(after)/1.349 # or scaled IQR

skewness(before)
kurtosis(before)

p1 <- hist(before)                     # centered at 4
p2 <- hist(after)                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second

require(ggplot2)
require(reshape2)
df <- data.frame(before, after)
ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")

