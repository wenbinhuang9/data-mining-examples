#######

library(neuralnet)
#Load Tinydata
cheese <- read.csv("Tinydata.csv", header=TRUE)
summary(cheese)
head(cheese)

#Create dummies for Acceptance
chees1 <- dummy.data.frame(cheese, sep=".")
summary(chees1)



cheesenn <- neuralnet(Acceptance.like + Acceptance.dislike ~ Salt + Fat, data = chees1, linear.output = F, hidden = 3)

cheesenn1 <- neuralnet(Acceptance.like + Acceptance.dislike ~ Salt + Fat, data = chees1, linear.output = F, hidden = c(3,2))

# display weights
cheesenn$weights

# display predictions
prediction(cheesenn)

# plot network
plot(cheesenn, rep="best")
plot(cheesenn1, rep="best")



#### 

library(caret)
Likepredict <- compute(cheesenn, data.frame(chees1$Salt, chees1$Fat))
Likepredict
predicted.class=apply(Likepredict$net.result,1,which.max)-1 #Predicted dislike
predicted.class 
confusionMatrix(as.factor(chees1$Acceptance.dislike),as.factor(predicted.class))
class(chees1$Acceptance.dislike)
class(predicted.class)

predict.Like <- ifelse(Likepredict$net.result[,1]>0.5,1,0)

confusionMatrix(as.factor(chees1$Acceptance.like),as.factor(predict.Like))

