interests <- read.csv("~/Desktop/fb_interest_distrib.csv", sep=";")
fb <- read.csv("~/Desktop/fb_friend_distrib.csv", sep=";")

hist(fb$count,breaks = 100,col = 3, freq=TRUE, main = 'Facebook # of Friends', xlab = 'Friends', ylab='# of Friends')
hist(interests$count,breaks = 100,col = 3, freq=TRUE, main = 'Facebook # of Interests', xlab = 'Users', ylab='# of Interests')

tab <- table(fb$count)
tab2 <- as.data.frame(cbind(as.integer(names(tab)), as.integer(tab)))
str(tab2)
plot(log(tab2$V1 ), log(tab2$V2 ), type = "l", lwd = 3, xlab="Log of friend count",ylab="Log of # of users")

tab <- table(interests$count)
tab2 <- as.data.frame(cbind(as.integer(names(tab)), as.integer(tab)))
str(tab2)
plot(log(tab2$V1 ), log(tab2$V2 ), type = "l", lwd = 3, xlab="Log of interests count",ylab="Log of # of users")

