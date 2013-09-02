# Author: Helena Buhr (Feb 20, 2013)
# Runs a logistic regression to estimate the fit of different features for people ranker
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
	directory_path <- '/Users/shill/Desktop/SMM/'
	setwd(directory_path) # set working directory
	getwd() # verify working directory

# Open click datafiles and zip code data (sepate files for zip code data)
	dat <- read.table("Fit_model_v1.txt", header = TRUE, sep = "\t") #  read data
	summary(dat)

# Recode vote to 0 and 1
	dat$vote <- ifelse(dat$vote == "N" | dat$vote == "No", 0, 1)
	summary(dat$vote)
	table(dat$vote) # look at the distribution of votes

# Recode mutual friend to 0 and 1
	dat$mutualfriend <- ifelse(dat$mutualfriend == "NO", 0, 1)
	summary(dat$mutualfriend)
	table(dat$mutualfriend) # look at the distribution of votes
	
	
# Generate a feature for age difference
	dat$age_diff = abs(dat$source_age - dat$target_age)

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

M1 <- with(dat, glm(vote ~ distance + age_diff + log(timesinceuse + 1) + mutualfriend
	, family=binomial("logit")))
summary(M1)
M1_likelihood <- with(M1, 
	y * log(fitted.values) + (1 - y) * log(1 - fitted.values) ) 
mean(M1_likelihood)

#--------------
# Inspect features
#----------------

par(mfrow = c(2, 2))

# Plot Effect of Distance
	
        # for each distance, probability of liking vs not liking
        temp1 <- table(round(dat$distance/ 100, 1), dat$vote)
        prob_data <- temp1[,2] / (temp1[,1] + temp1[,2])
        plot(as.numeric(rownames(temp1)), prob_data, type = "p", ylab = "probability of yes click", xlab = "distance between user and target (in 100 km)",
        main = "Probability of Yes", ylim = c(0.00, 1), xlim = c(0, 2))
        lines(prob_data ~ as.numeric(rownames(temp1)))


# Plot effect of age difference

      temp2 <- table(round(dat$age_diff), dat$vote)
        prob_data <- temp2[,2] / (temp2[,1] + temp2[,2])
        plot(as.numeric(rownames(temp2)), prob_data, type = "p", ylab = "probability of yes click", xlab = "Absolute age difference between user and target",
        main = "Probability of Yes", ylim = c(0.00, 1), xlim = c(0, 10))
        lines(prob_data ~ as.numeric(rownames(temp2)))


# Effect of mutual friend
      temp3 <- table(round(dat$mutualfriend), dat$vote)
        prob_data <- temp3[,2] / (temp3[,1] + temp3[,2])
        barplot(prob_data, ylab = "probability of yes click", xlab = "Mutual friend: Yes = 1. No = 0",
        main = "Probability of Yes", ylim = c(0.00, 1))
#        lines(prob_data ~ as.numeric(rownames(temp3)))

# Minutes since the target used the app
   
     temp4 <- table(round(log(dat$timesinceuse + 1), 0), dat$vote)
        prob_data <- temp4[,2] / (temp4[,1] + temp4[,2])
        plot(as.numeric(rownames(temp4)), prob_data, type = "p", ylab = "probability of yes click", xlab = "Minutes since target used the app: log(minutes + 1)",
        main = "Probability of Yes", ylim = c(0.00, 1))
        lines(prob_data ~ as.numeric(rownames(temp4)))


 
 
 