d <- read.csv("~/Documents/R Scripts/Data/Decay function.csv")
plot(d)

m.users <- lm(log(d$y) ~ log(d$x), data = d);
m.users
x<-1:365
plot(x,exp(-0.9435*log(x)+11.8864 ),type="l",main="Fitting power law to 30 days of sample data",xlab="days",ylab="users triggered")
lines(d$x,d$y, col='red');