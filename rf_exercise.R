library(randomForest)

authorship <- read.csv("~/Documents/R Scripts/data-acd/authorship.csv")

##create a training set
authorship$randu = runif(841, 0,1)
authorship.train = authorship[authorship$randu < .4,]
authorship.test = authorship[authorship$randu >= .4,]
authorship.model.rf = randomForest(Author ~ a + all + also + an + any + are + as + at + be + been + but + by + can + do + down + even + every + for. + from + had + has + have + her + his + if. + in. + into + is + it + its + may + more + must + my + no + not + now + of + on + one + only + or + our + should + so + some + such + than + that + the + their + then + there + things + this + to + up + upon + was + were + what + when + which + who + will + with + would + your, data=authorship.train, ntree=5000, mtry=15, importance=TRUE)
authorship.test$pred.author.rf = predict(authorship.model.rf, authorship.test, type="response")
table(authorship.test$Author, authorship.test$pred.author.rf)
prop.table(table(authorship.test$Author, authorship.test$pred.author.rf),1)