
# Runs a logistic regression to estimate the fit of different features for contact closeness
# vote outcome model 
# Updated: Helena Buhr (June 10, 2013)

#---------
# Set up
#---------
# Remove old data
delete_old <- function()
  rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)
delete_old()
ls()
# Set working directory
directory_path <- '/Users/shill/documents/R scripts/Mobile Viral Growth'
setwd(directory_path) # set working directory
getwd() # verify working directory

# Open click datafiles and zip code data (sepate files for zip code data)
dat <- read.table("contact_data.csv", header = TRUE, sep = ",") #  read data
#dat <- subset(dat, dat$gender == "F")
summary(dat)

#areaCode
#vote Other (=0), Friend (=1), Family (=2)
#lastCallDateTime
#timeCalled
#textMsgs
#callFreq
#starred
#hasPhoto
#isFirstName
#callRecency
#areaCodeDist
#
#dat<-subset(dat, dat$gender=="F")

# Select which version to test
#dat$vote <- ifelse(dat$vote == "2" | dat$vote == "1", 1,0) #combine friend and family
#dat$vote <- ifelse(dat$vote == "2", 1,0) #just family
dat$friend <- ifelse(dat$vote == "1", 1,0) #just friend
dat$friend <- ordered(dat$friend,
                      levels = c(0,1),
                      labels = c("Not Friend", "Friend"))
dat$family <- ifelse(dat$vote == "2", 1,0) 

dat$outcome <- ordered(dat$vote,
                       levels = c(0,1, 2),
                       labels = c("Other", "Friend", "Family"))

# Descriptives by user
descriptives <- table(dat$testSubject, dat$friend)
descriptives <- cbind(descriptives, descriptives[,2] / (descriptives[,2] + descriptives[,1]),  (descriptives[,2] + descriptives[,1]))
par(mfrow=c(1,2))
plot( descriptives[,4],  descriptives[,2] , xlab = "Total Contacts", ylab = "Friends") 
abline(lm(descriptives[,2] ~ descriptives[,4]) , col = 'red', lty = 2)
plot(descriptives[,4] ,  descriptives[,3] , xlab = "Total Contacts", ylab = " % Friends in contact list") # share friends
abline(lm(descriptives[,3] ~ descriptives[,4]), col = 'red', lty = 2)

# Descriptives for friends / non friends   

# Text vs calls : Exclude zero values
callFreqfriends <- table(dat$callFreq, dat$friend)
par(mfrow=c(1,2))
datSub <- subset(dat, dat$callFreq > 0 ) # Exclude zero values
boxplot(log(datSub$callFreq + 1)  ~ datSub$outcome, ylab="Call Frequncy", main = "Calls (zero values excl.)")

datSub <- subset(dat, dat$textMsgs > 0 ) # Exclude zero values
boxplot(log(datSub$textMsgs + 1)  ~ datSub$outcome, ylab="Text Frequncy", main = "Text (zero values excl.)")
# Text vs calls     
par(mfrow=c(1,2))
plot(dat$callFreq , dat$textMsgs , xlab = "Text messages", ylab = " Calls") # share friends
abline(lm(dat$textMsgs ~ dat$callFreq), col = 'red', lty = 2)
plot(log(dat$callFreq +1) , log(dat$textMsgs + 1) , xlab = "Text messages (log)", ylab = " Calls (log)") # share friends
abline(lm(log(dat$textMsgs + 1)  ~ log(dat$callFreq + 1) ), col = 'red', lty = 2)
# Text to call ratio
par(mfrow=c(1,1))
dat$text2cal <-  dat$textMsgs /  (dat$callFreq + 1)
boxplot( log(dat$text2cal + 1)  ~dat$outcome, ylab="Text / call ratio (logged)", main = "Text / call ratio (logged)")
par(mfrow=c(1,3))
hist(log(subset(dat, dat$vote == 0)$text2cal + 1), main = "Other", xlab = "Text to call ratio (logged)")
hist(log(subset(dat, dat$vote == 1)$text2cal + 1), main = "Friends",  xlab = "Text to call ratio (logged)")
hist(log(subset(dat, dat$vote == 2)$text2cal + 1), main = "Family",  xlab = "Text to call ratio (logged)")
# People without any contacts
dat$textMsgsBinary <- ifelse(dat$textMsgs > 0, 1, 0)  
textBinary <- table(dat$outcome, dat$textMsgsBinary) 
textBinary <- cbind(textBinary, (textBinary[,2] / (textBinary[,1] + textBinary[,2] )  ))
barplot(textBinary[,3], main = "Text", ylab = "Fraction with at least one text msg")

dat$callFreqBinary <- ifelse(dat$callFreq > 0, 1, 0)  
callBinary <- table(dat$outcome, dat$callFreqBinary) 
callBinary <- cbind(callBinary, (callBinary[,2] / (callBinary[,1] + callBinary[,2] )  ))
barplot(callBinary[,3], main = "Calls", ylab = "Fraction with at least one call")

# What does it mean if you text and not call and vice versa
par(mfrow=c(2,2))
# no text and no call
dat$noTextnoCall <- ifelse(dat$textMsgsBinary == 0 & dat$callFreqBinary == 0, 1, 0)  
temp <- table(dat$outcome, dat$noTextnoCall) 
temp <- cbind(temp, (temp[,2] / (temp[,1] + temp[,2] )  ))
barplot(temp[,3], main = "No Text & No Call", ylab = "Fraction of users")
rm(temp)

# Call and no text
dat$noTextCall <- ifelse(dat$textMsgsBinary == 0 & dat$callFreqBinary == 1, 1, 0)  
temp <- table(dat$outcome, dat$noTextCall) 
temp <- cbind(temp, (temp[,2] / (temp[,1] + temp[,2] )  ))
barplot(temp[,3], main = "No Text, but Call", ylab = "Fraction of users")
rm(temp)

# Text and no call
dat$TextNoCall <- ifelse(dat$textMsgsBinary == 1 & dat$callFreqBinary == 0, 1, 0)  
temp <- table(dat$outcome, dat$TextNoCall) 
temp <- cbind(temp, (temp[,2] / (temp[,1] + temp[,2] )  ))
barplot(temp[,3], main = "Text, but No Call", ylab = "Fraction of users")
rm(temp)

# Text and call 
dat$TextCall <- ifelse(dat$textMsgsBinary == 1 & dat$callFreqBinary == 1, 1, 0)  
temp <- table(dat$outcome, dat$TextCall) 
temp <- cbind(temp, (temp[,2] / (temp[,1] + temp[,2] )  ))
barplot(temp[,3], main = "Text & Call", ylab = "Fraction of users")
rm(temp)

# Correlations between text and calls
par(mfrow=c(1,2))
plot(dat$callFreq, dat$textMsgs, xlab = "Calls", ylab = "Text msgs")
abline(lm(dat$textMsgs ~ dat$callFreq), col = 'red', lty = 2)
plot(log(dat$callFreq + 1), log(dat$textMsgs + 1), xlab = "Calls (logged)", ylab = "Text msgs (logged)")
abline(lm(log(dat$textMsgs + 1) ~ log(dat$callFreq + 1)), col = 'red', lty = 2)  

dat$starred <- ifelse(dat$starred == 1, 1,0) #fill in missing false values
dat$hasPhoto <- ifelse(dat$hasPhoto == 1, 1,0) #fill in missing false values

# Descriptive statistics
summary(dat)
#-------------------
# Subset the data set
# Exclude family since they will be excluded with other rules
#--------------

dat <- subset(dat, dat$family == 0)


#--------------
# Baseline model 
#---------

M0 <- with(dat, glm(vote ~ 1
                    , family=binomial("logit")))
summary(M0)
M0_likelihood <- with(M0, 
                      y * log(fitted.values) + (1 - y) * log(1 - fitted.values) ) 
mean(M0_likelihood)

#--------------
# Add features 
#----------------

# callFreq   + callFreqBinary
# log(textMsgs + 1) + textMsgsBinary 

M1 <- with(dat, glm(vote ~ textMsgs  + textMsgsBinary  +hasPhoto + starred 
                    , family=binomial("logit")))
summary(M1)

M1_likelihood <- with(M1, 
                      y * log(fitted.values) + (1 - y) * log(1 - fitted.values) ) 
mean(M1_likelihood)
dat$fitted <- ifelse(M1$fitted < 0.5, 0, 1)
table(dat$fitted, dat$friend)

#--------------
# No text data
#----------------

# callFreq   + callFreqBinary
# log(textMsgs + 1) + textMsgsBinary 

M1 <- with(dat, glm(vote ~  hasPhoto + starred 
                    , family=binomial("logit")))
summary(M1)

M1_likelihood <- with(M1, 
                      y * log(fitted.values) + (1 - y) * log(1 - fitted.values) ) 
mean(M1_likelihood)
dat$fitted <- ifelse(M1$fitted < 0.5, 0, 1)
table(dat$fitted, dat$friend)








