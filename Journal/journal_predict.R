library(randomForest)
setwd('/Users/shill/documents/R scripts')

journal.train <- read.csv("Journal/journal_train3.csv")

journal.test<-data.frame(Mood=c('Tired','Interested'), Activity=c(10,9), Company=c(4,3), Situation=c(1,1), Thinking=c(2,1), Public=c(1,2), Day=c(2,1),Time=c(9,7))

journal.model.rf = randomForest(Mood ~ Activity+Company+Situation+Thinking+Public+Day+Time, data=journal.train, ntree=501, mtry=6, importance=TRUE)
journal.test$pred.mood.rf = predict(journal.model.rf, journal.test, type="response")
#table(journal.test$Mood, journal.test$pred.mood.rf);
prop.table(table(journal.test$Mood, journal.test$pred.mood.rf),1)
