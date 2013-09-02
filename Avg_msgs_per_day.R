setwd('/Users/shill/documents/R scripts')

msgs <- read.csv("avg_msg_per_day.csv", sep=",")
hist(msgs$avg,breaks = 100,col = 3, freq=TRUE, main = 'Average Push Messages Per User Per Day', xlab = 'Day Count', ylab='# Messages')

summary(msgs$avg)