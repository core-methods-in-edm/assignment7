library(tidyr, dplyr)
library(corrplot)
library(ggplot2)
library(rpart)
library(ROCR)

#Start by creating histograms of the distributions for all variables (#HINT: look up "facet" in the ggplot documentation)

D1 <- read.table("online.data.csv", sep = ",", header = TRUE)
D1$level.up2<-ifelse(D1$level.up=="no",0,1)
D2 <- dplyr::select(D1, 2:6,8)
D2 <- tidyr::gather(D2)
names(D2) <- c("variable","value")
g <- ggplot(D2,aes(x=value))
#Facets divide a plot into subplots based on the values of one or more discrete variables
#g+geom_histogram(binwidth = diff(range(D2$value))/2000)+facet_wrap(~variable, scales = "free")
#code reference:  https://groups.google.com/forum/#!topic/ggplot2/rhPWQEFMx6A

g + geom_histogram(data = D2[D2$variable == "av.assignment.score",], binwidth=0.01) +
  geom_histogram(data = D2[D2$variable == "forum.posts",], binwidth=1) +  
  geom_histogram(data = D2[D2$variable == "level.up2",], binwidth=1) +
  geom_histogram(data = D2[D2$variable == "messages",], binwidth=1) + 
  geom_histogram(data = D2[D2$variable == "post.test.score",], binwidth=0.01) +
  geom_histogram(data = D2[D2$variable == "pre.test.score",], binwidth=0.01) + 
  facet_wrap(~variable, scales = "free")
#code reference: http://stackoverflow.com/questions/17271968/different-breaks-per-facet-in-ggplot2-histogram by user20650

#Then visualize the relationships between variables
D3 <- dplyr::select(D1, 2:6,8)
COR <- cor(D3)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.01, insig = "blank")

#Try to capture an intution about the data and the relationships
#The post test score is highly related to the number of messages(0.94), and pre test score/prior knowledge(67), assignment score(76). the post test score is not the simple cut-line for level-up. 
#I guess level-up is based on students' improvement = postTestScore - preTestScore, or a total = postTestScore + preTestScore, or a combination of the most related variables to level.up2: messages + post.test.score + av.assignment.score
#D3$change <- D3$post.test.score - D3$pre.test.score
#D3$total <- D3$post.test.score + D3$pre.test.score

#Create a classification tree that predicts whether a student "levels up" in the online course using three variables of your choice (As we did last time, set all controls to their minimums)
#Sample <- dplyr::sample_frac(D1, 0.1, replace = TRUE)
c.tree1 <- rpart(level.up ~ messages + post.test.score + av.assignment.score, method="class", data=D1,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
#Plot and generate a CP table for your tree 
post(c.tree1, file = "tree1.ps", title = "tree1")
printcp(c.tree1)
#Classification tree:
#rpart(formula = level.up ~ messages + post.test.score + av.assignment.score, 
#    data = D1, method = "class", control = rpart.control(minsplit = 1, 
#        minbucket = 1, cp = 0.001))
#Variables actually used in tree construction:
#[1] av.assignment.score post.test.score    
#Root node error: 400/1000 = 0.4
#n= 1000 
#     CP nsplit rel error xerror     xstd
#1 0.930      0      1.00   1.00 0.038730
#2 0.070      1      0.07   0.07 0.013042
#3 0.001      2      0.00   0.00 0.000000

#Generate a probability value that represents the probability that a student levels up based your classification tree 
#Based on the classification tree1, the probability that a student level up is 400/1000 = 40%

D1$pred <- predict(c.tree1, type = "prob")[,2]
#Last class we used type = "class" which predicted the classification for us, this time we are using type = "prob" to see the probability that our classififcation is based on.
#mismatch1 <- dplyr::filter(D1, level.up2 != pred)

#Now you can generate the ROC curve for your model. You will need to install the package ROCR to do this.
#Plot the curve
pred1 <- prediction(D1$pred, D1$level.up) 
plot(performance(pred1, "tpr", "fpr"),colorize=TRUE)
abline(0, 1, lty = 2)

#Calculate the Area Under the Curve
unlist(slot(performance(pred1,"auc"), "y.values"))#Unlist liberates the AUC value from the "performance" object created by ROCR
#[1] 1

#Now repeat this process, but using the variables you did not use for the previous model and compare the plots & results of your two models. Which one do you think was the better model? Why?
#alright, let's have 3 of the least related variables to level.up2 to construct the second classification tree: forum.posts+pre.test.score+message

c.tree2 <- rpart(level.up ~ forum.posts + pre.test.score + messages, method="class", data=D1)
#Plot and generate a CP table for your tree 
post(c.tree2, file = "tree2.ps", title = "tree2")
printcp(c.tree2)
#Variables actually used in tree construction:
#  [1] messages       pre.test.score
#Root node error: 400/1000 = 0.4
#n= 1000 
#CP nsplit rel error xerror     xstd
#1 0.54250      0    1.0000 1.0000 0.038730
#2 0.01125      1    0.4575 0.4675 0.030825
#3 0.01000      3    0.4350 0.4750 0.031014

post(c.tree2, file = "tree2.ps", title = "tree2")
D1$pred2 <- predict(c.tree2, type = "prob")[,2]
#Based on the classification tree, the probability that a student level up is 
prob2 <- (147+211)/1000 
prob2 # [1] 0.358
Pred2 <- prediction(D1$pred2, D1$level.up) 
plot(performance(Pred2, "tpr", "fpr"),colorize=TRUE)

abline(0, 1, lty = 2)
unlist(slot(performance(Pred2,"auc"), "y.values"))
#[1] 0.8825125


#Look at the ROC plot for your first model. Based on this plot choose a probability threshold that balances 
#capturing the most correct predictions against false positives. Then generate a new variable in your data set 
#that classifies each student according to your chosen threshold.

#Model1: from ROC1, we can set threshold.pred1 as 0
threshold.pred1 = 0
D1$threshold.pred1 <- ifelse(D1$pred <= threshold.pred1, "no","yes") #I had no idea about this step, searching online for hours, finally got hints from Edward and your collaboration. Thank you two.
  #Now generate three diagnostics:
table1 <- table(D1$level.up, D1$threshold.pred1)
table1
#     no yes
#no  600   0
#yes   0 400
  accuracy1 <- (400+600)/(400+600) # 100%
  precision1 <- 400/(400+0) #100%
  recall1 <- 400/(400+0) #100%
  
  #Model2: from ROC2, we can set threshold.pred2 as 0.5
  threshold.pred2 = 0.5
  D1$threshold.pred2 <- ifelse(D1$pred2 <= threshold.pred2, "no","yes") 
  table2 <- table(D1$level.up, D1$threshold.pred2)
  table2
 #      no yes
 # no  468 132
 # yes  42 358
  accuracy2 <- (468+358)/(468+358+132+42) # 82.6%
  precision2 <- 358/(358+132) #73.06122%
  recall2 <- 358/(358+42) #89.5%
   
  #Finally, calculate Kappa for your model according to:
  
  #First generate the table of comparisons
  table1 <- table(D1$level.up, D1$threshold.pred1)

#Convert to matrix
matrix1 <- as.matrix(table1)

#Calculate kappa
kappa(matrix1, exact = TRUE)/kappa(matrix1)
#[1] 1.153846  ???? 

#hand calculation for Model1 
#Po = 1000/1000 = 1 
#Pe = (600/1000)* (600/1000) + (400/1000)*(400/1000)=0.36+0.16=0.52
#kappa = (1-0.52)/(1-0.52) = 1

#Now choose a different threshold value and repeat these diagnostics. What conclusions can you draw about your two thresholds?
#For Model1, choosing another threshold not equal to 0.

threshold.pred1 = 0.999
D1$threshold.pred1 <- ifelse(D1$pred <= threshold.pred1, "no","yes")
table1 <- table(D1$level.up, D1$threshold.pred1)
table1
#     no yes
#no  600   0
#yes   0 400
#still geting the perfect prediction.....Maybe I should comepare Model 2 threshold 1 and threshold 2
accuracy1 <- (400+600)/(400+600) # 100%
precision1 <- 400/(400+0) #100%
recall1 <- 400/(400+0) #100%

#Model2: from ROC2, we set the threshold.pred2b as 0.55
threshold.pred2b = 0.4
D1$threshold.pred2b <- ifelse(D1$pred2 <= threshold.pred2b, "no","yes") 
table2b <- table(D1$level.up, D1$threshold.pred2b)
table2b
#     no yes
#no  440 160
#yes  23 377
accuracy2 <- (440+377)/(440+377+160+23) # 81.7%
precision2 <- 377/(377+160) #70.20484%
recall2 <- 377/(377+23) #94.25%
#We get higher percentage of recall rate with threshold 2, having lower accuracy and percision rate as trade off. 
#It depends on the context to determine the threshold. ex.In medical, recall rate is more important. 

