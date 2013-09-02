
# Runs a logistic regression to estimate the fit of different features for contact closeness
# vote outcome model 

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
dat$vote <- ifelse(dat$vote == "2" | dat$vote == "1", 1,0) #combine friend and family
#dat$vote <- ifelse(dat$vote == "2", 1,0) #just family
#dat$vote <- ifelse(dat$vote == "1", 1,0) #just friend


dat$starred <- ifelse(dat$starred == 1, 1,0) #fill in missing false values
dat$hasPhoto <- ifelse(dat$hasPhoto == 1, 1,0) #fill in missing false values
dat$textMsgs <- log(dat$textMsgs+1) #convert # of text messages to log
#	summary(dat$vote)
#	table(dat$vote) # look at the distribution of votes

# Recode mutual friend to 0 and 1
#	dat$mutualfriend <- ifelse(dat$mutualfriend == "NO", 0, 1)
#	summary(dat$mutualfriend)
#	table(dat$mutualfriend) # look at the distribution of votes
	
	
# Generate a feature for age difference
#	dat$age_diff = abs(dat$source_age - dat$target_age)

# Descriptive statistics
	summary(dat)

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

M1 <- with(dat, glm(vote ~ areaCodeDist +hasPhoto+starred+callRecency+textMsgs+callFreq
	, family=binomial("logit")))
summary(M1)
M1_likelihood <- with(M1, 
	y * log(fitted.values) + (1 - y) * log(1 - fitted.values) ) 
mean(M1_likelihood)

dat$fitted <- ifelse(M1$fitted < 0.5, 0, 1)
table(dat$fitted, dat$vote)

#--------------
# Inspect features
#----------------

par(mfrow = c(2, 2))

# Plot Effect of areaCodeDist
	
    # for each distance, probability of liking vs not liking
    temp1 <- table(round(dat$areaCodeDist, 1), dat$vote)
    prob_data <- temp1[,2] / (temp1[,1] + temp1[,2])
    plot(as.numeric(rownames(temp1)), prob_data, type = "p", ylab = "probability of true", xlab = "log of difference in area code",
    main = "Probability of Area Code Distance", ylim = c(0.00, 1), xlim = c(0, 3))
    lines(prob_data ~ as.numeric(rownames(temp1)))


# Plot effect of callRecency
    temp2 <- table(round(dat$callRecency, 1), dat$vote)
    prob_data <- temp2[,2] / (temp2[,1] + temp2[,2])
    plot(as.numeric(rownames(temp2)), prob_data, type = "p", ylab = "probability of true", xlab = "log(366- call recency)",
         main = "Probability of Call Recency", ylim = c(0.00, 1), xlim = c(0, 3))
    lines(prob_data ~ as.numeric(rownames(temp2)))


# Effect of callFreq
    temp3 <- table(round(dat$callFreq, 1), dat$vote)
    prob_data <- temp3[,2] / (temp1[,1] + temp3[,2])
    plot(as.numeric(rownames(temp3)), prob_data, type = "p", ylab = "probability of true", xlab = "call frequency",
         main = "Probability of Call Frequency", ylim = c(0.00, 1), xlim = c(0, 100))
    lines(prob_data ~ as.numeric(rownames(temp3)))


# Effect of talkTime
    temp4 <- table(round(dat$talkTime, 1), dat$vote)
    prob_data <- temp4[,2] / (temp4[,1] + temp4[,2])
    plot(as.numeric(rownames(temp4)), prob_data, type = "p", ylab = "probability of true", xlab = "talk time",
         main = "Probability of Talk Time", ylim = c(0.00, 1), xlim = c(0, 30))
    lines(prob_data ~ as.numeric(rownames(temp4)))


# Effect of textMsgs
    temp5 <- table(round(dat$textMsgs, 1), dat$vote)
    prob_data <- temp5[,2] / (temp5[,1] + temp5[,2])
    plot(as.numeric(rownames(temp5)), prob_data, type = "p", ylab = "probability of true", xlab = "text msgs",
         main = "Probability of Text Msgs", ylim = c(0.00, 1), xlim = c(0, 10))
    lines(prob_data ~ as.numeric(rownames(temp5)))


# Effect of hasPhoto
temp6 <- table(round(dat$hasPhoto, 1), dat$vote)
prob_data <- temp6[,2] / (temp6[,1] + temp6[,2])
plot(as.numeric(rownames(temp6)), prob_data, type = "p", ylab = "probability of true", xlab = "text msgs",
     main = "Probability of Has Photo", ylim = c(0.00, 1), xlim = c(0, 1))
lines(prob_data ~ as.numeric(rownames(temp6)))

# Effect of starred
temp7 <- table(round(dat$starred, 1), dat$vote)
prob_data <- temp7[,2] / (temp7[,1] + temp7[,2])
plot(as.numeric(rownames(temp7)), prob_data, type = "p", ylab = "probability of true", xlab = "text msgs",
     main = "Probability of Starred", ylim = c(0.00, 1), xlim = c(0, 1))
lines(prob_data ~ as.numeric(rownames(temp7)))

# Effect of isFirstName
temp8 <- table(round(dat$isFirstName, 1), dat$vote)
prob_data <- temp8[,2] / (temp8[,1] + temp8[,2])
plot(as.numeric(rownames(temp8)), prob_data, type = "p", ylab = "probability of true", xlab = "text msgs",
     main = "XXXX IGNORE XXXX", ylim = c(0.00, 1), xlim = c(0, 1))
lines(prob_data ~ as.numeric(rownames(temp8)))
 
 
 