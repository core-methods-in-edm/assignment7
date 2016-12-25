#Start by creating histograms of the distributions for all variables (#HINT: look up "facet" in the ggplot documentation)

#Then visualize the relationships between variables

#Try to capture an intution about the data and the relationships


read.csv("online.data.csv")

D1 <- read.csv("online.data.csv")

library(reshape2)

library(ggplot2)

D1$level.up <- ifelse(D1$level.up == "no",0,1)

#need to scale the data, but make sure to set it up as.data.frame

D1<- as.data.frame(scale(D1))

D2 <- dplyr:: select(D1, -id)

D2 <- tidyr:: gather(D2, "variable", "value", 1:6)

as.numeric(as.character(D2$value))

#facet
library(ggplot2)

hist1 = ggplot(D2,aes(x=value)) + geom_histogram(binwidth = 0.1) + facet_wrap(~variable, scales = "free")

#Then visualize the relationships between variables
library(corrplot)

pairs(D1)
cor1 <- cor(D1)
corrplot(cor1, order="AOE", method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.6, tl.srt=45, addCoef.col="black", addCoefasPercent = TRUE, sig.level=0.50, insig = "blank")

#Try to capture an intution about the data and the relationships
#The strongest correlation is between messages and post test score. Average assignment score has a high correlation with level up
#Classification tree
```{r}
#Create a classification tree that predicts whether a student "levels up" in the online course using three variables of your choice (As we did last time, set all controls to their minimums)
library(rpart)
c.tree1 <- rpart(level.up ~ messages + forum.posts + av.assignment.score, method="class" , data = D1, control=rpart.control(minsplit=1,minbucket=1,cp=0.001))

printcp(c.tree1)
#Plot and generate a CP table for your tree 
plot(c.tree1)
text(c.tree1)

#Generate a probability value that represents the probability that a student levels up based your classification tree 

D1$pred <- predict(rp, type = "prob")[,2]#Last class we used type = "class" which predicted the classification for us, this time we are using type = "prob" to see the probability that our classififcation is based on.

library(ROCR)
#Plot the curve
pred.detail <- prediction(D1$pred, D1$level.up) 
plot(performance(pred.detail, "tpr", "fpr"))
abline(0, 1, lty = 2)

#Calculate the Area Under the Curve
unlist(slot(performance(Pred2,"auc"), "y.values"))#Unlist liberates the AUC value from the "performance" object created by ROCR

#Now repeat this process, but using the variables you did not use for the previous model and compare the plots & results of your two models. Which one do you think was the better model? Why?





