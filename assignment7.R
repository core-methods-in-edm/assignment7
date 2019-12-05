library(tidyverse)
library(caTools)
library(rpart)
library(party)
library(rpart.plot)
library(randomForest)
library(zoo)


testscores <- online_data
colnames(testscores)
# [1] "id"                  "post.test.score"     "pre.test.score"      "messages"            "forum.posts"        
# [6] "av.assignment.score"  "level.up"     

library(ggplot2)

ggplot(testscores, aes(x = post.test.score, y = pre.test.score, color = messages)) +
  geom_point()

# more messages higher post.test.score

prediction <- rpart(level.up ~ messages + forum.posts + pre.test.score, 
                      method = 'class', data = testscores)
prediction
prp(prediction)

testscores$pred <- predict(prediction, type = "prob")[,2]

library(ROCR)

pred.detail <- prediction(testscores$pred, testscores$level.up) 
plot(performance(pred.detail, "tpr", "fpr"))
abline(0, 1, lty = 2)

unlist(slot(performance(Pred2,"auc"), "y.values"))
  
##


testscores$threshold.pred1 <- ifelse(testscores$pred >= 0.4, 1, 0)

data$TPOS.model1 <- ifelse(testscores$level.up == "1" & testscores$threshold.pred1 == "1", 
                              1, 0)
testscores$FPOS.model1 <- ifelse(testscores$level.up == "0" & testscores$threshold.pred1 == "1",
                               1, 0)
testscores$FNEG.model1 <- ifelse(testscores$level.up == "1" & testscores$threshold.pred1 == "0",
                               1, 0)

#Now generate three diagnostics:

testscores$accuracy.model1 <- mean(ifelse(testscores$level.up == testscores$threshold.pred1, 1, 0))

testscores$precision.model1 <- sum(testscores$TPOS.model1)/(sum(data$TPOS.model1) 
                                                     + sum(data$FPOS.model1))

testscores$recall.model1 <- sum(testscores$TPOS.model1)/(sum(testscores$TPOS.model1) 
                                                  + sum(testscores$FNEG.model1))

#Finally, calculate Kappa for your model according to
#First generate the table of comparisons

table1 <- table(testscores$level.up, testscores$threshold.pred1)
table1 <- table(testscores$level.up, testscores$threshold.pred1)

#Convert to matrix
matrix1 <- as.matrix(table1)
kappa(matrix1, exact = TRUE)/kappa(matrix1)

#Now choose a different threshold value and repeat these diagnostics. What conclusions can you draw about your two thresholds?
testscores$threshold.pred2 <- ifelse(testscores$pred >= 0.8, 1, 0)
testscores$TPOS.model2 <- ifelse(testscores$level.up == "1" & testscores$threshold.pred2 == "1", 
                              1, 0)
testscores$FPOS.model2 <- ifelse(testscores$level.up == "0" & testscores$threshold.pred2 == "1",
                               1, 0)
testscores$FNEG.model2 <- ifelse(testscores$level.up == "1" & testscores$threshold.pred2 == "0",
                               1, 0)
testscores$accuracy.model2 <- mean(ifelse(testscores$level.up == testscores$threshold.pred2, 1, 0))
testscores$precision.model2 <- sum(testscores$TPOS.model2)/(sum(testscores$TPOS.model2) 
                                                   + sum(testscores$FPOS.model2))

testscores$recall.model2 <- sum(testscores$TPOS.model2)/(sum(testscores$TPOS.model2) 
                                                + sum(testscores$FNEG.model2))

table2 <- table(testscores$level.up, testscores$threshold.pred2)
matrix2 <- as.matrix(table2)
kappa(matrix2, exact = TRUE)/kappa(matrix2)