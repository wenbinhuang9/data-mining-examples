#logistic model for bBayAuctions
#Q1.A Develop a logit model with all the predictors.
ebay.df <- read.csv("eBayAuctions.csv", header = TRUE)

summary(ebay.df)
dim(ebay.df)
#partiton
set.seed(1)
train.rows <- sample(rownames(ebay.df), dim(ebay.df)[1]*0.6)
ebaytrain.df <- ebay.df[train.rows, ]
valid.rows <- setdiff(rownames(ebay.df), train.rows) 
ebayvalid.df <- ebay.df[valid.rows, ]

dim(ebaytrain.df)
dim(ebayvalid.df)

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
ebaylogit.reg <- glm(Competitive. ~ ., data = ebaytrain.df , family = "binomial") #create the model
options(scipen=999)
summary(ebaylogit.reg)


#Q1.B  Compare the predictive accuracy in your validation data with that of what was obtained
#in HW 3 (using classification tree).

#t(t(names(ebayvalid.df)))
# use predict() with type = "response" to compute predicted probabilities on my validation data.
ebaylogit.reg.pred <- predict(ebaylogit.reg, ebayvalid.df[, -8], type = "response") 

# take a look at first 5 actual and predicted records
data.frame(actual.Competitive     =  ebayvalid.df$Competitive.[1:5], 
           predicted.competitive  =  ebaylogit.reg.pred[1:5])

# use gain table to see how my model performance on validation data
library(gains)
ebaygain <- gains(ebayvalid.df$Competitive., ebaylogit.reg.pred, groups=10)
ebaygain

# plot lift chart
plot(c(0,ebaygain$cume.pct.of.total*sum(ebayvalid.df$Competitive.))~c(0,ebaygain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")

lines(c(0,sum(ebayvalid.df$Competitive.))~c(0, dim(ebayvalid.df)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- ebaygain$mean.resp/mean(ebayvalid.df$Competitive.)
heights
decileplot <- barplot(heights, names.arg = ebaygain$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(decileplot, heights+0.5, labels=round(heights, 1), cex = 0.8)# add labels to columns

#generate confusion matrix
confusionMatrix(as.factor(ifelse(ebaylogit.reg.pred>0.5,1,0)), as.factor(ebayvalid.df[,8]))#here 0.5 is cutoff value we use

############################ use classification tree(also use all the predictors which is not useful to predict on new data)
library(rpart)
library(rpart.plot)
library(caret)

# use cross validation to create a tree
ebaycv.ct <- rpart(Competitive. ~ ., data = ebaytrain.df, method = "class", 
                   control = rpart.control(minsplit=50, maxdepth = 7),
                   cp = 0.00001, xval=5)
printcp(ebaycv.ct)

# prune my tree
ebaypruned.ct <- prune(ebaycv.ct , cp = 0.018)
# find length for pruned tree
length(ebaypruned.ct$frame$var[ebaypruned.ct$frame$var == "<leaf>"])
# plot this pruned tree
prp(ebaypruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(ebaypruned.ct$frame$var == "<leaf>", 'gray', 'white'))
#check accuaracy on valid
ebaypruned.ct.pred.valid <- predict(ebaypruned.ct,ebayvalid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(ebaypruned.ct.pred.valid, as.factor(ebayvalid.df$Competitive.))

## for CT
## accuracy = 0.8238
## sensitivity = 0.9398
## specificity = 0.7150

## for logistic
## accuracy = 0.7782
## sensitivity = 0.7775
## specificity = 0.7789

#Q1.C
#install.packages("glmulti")

#Q1.D Which predictors would you use based on predictive accuracy?
#Category, currency, sellerRating,Duration, endDay,ClosePrice, OpenPrice,Competitive.
#Category, sellerRating,OpenPrice,

ebaylogit2.reg <- glm(Competitive. ~ Category+sellerRating+OpenPrice, data = ebaytrain.df , family = "binomial") #create the model
options(scipen=999)
summary(ebaylogit2.reg)
# use predict() with type = "response" to compute predicted probabilities on my validation data.
ebaylogit2.reg.pred <- predict(ebaylogit2.reg, ebayvalid.df[, -8], type = "response") 
# use gain table to see how my model performance on validation data
library(gains)
ebaygain2 <- gains(ebayvalid.df$Competitive., ebaylogit2.reg.pred, groups=10)
ebaygain2
# plot lift chart
plot(c(0,ebaygain2$cume.pct.of.total*sum(ebayvalid.df$Competitive.))~c(0,ebaygain2$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")

lines(c(0,sum(ebayvalid.df$Competitive.))~c(0, dim(ebayvalid.df)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- ebaygain2$mean.resp/mean(ebayvalid.df$Competitive.)
heights
decileplot <- barplot(heights, names.arg = ebaygain2$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(decileplot, heights+0.5, labels=round(heights, 1), cex = 0.8)# add labels to columns
#generate confusion matrix
confusionMatrix(as.factor(ifelse(ebaylogit2.reg.pred>0.5,1,0)), as.factor(ebayvalid.df[,8]))#here 0.5 is cutoff value we use


#Q1.E   Interpret the meaning of the coefficient for closing price？
#closeprice estimate:0.10877929
# if closing price increase by $1, 
# the odds(Competitive = 1 | Closing price) will increase by a factor of e^0.10877929 .

#Q1.F Predict the odds for a new data based on your final model (You are welcome to choose
# your own values for the new data. Please clearly state it in your report)
new.df <- ebay.df[444,]
new.df

ebaylogit3.reg.pred <- predict(ebaylogit2.reg, new.df[, -8], type = "response") 
ebaylogit3.reg.pred 
new.df

#Q2.ADevelop a neural network to predict Price. You may use the same predictors we had
#used in class (while discussing multivariable regression). Start with one hidden layer
#with 2 nodes. Make sure you scale the data.

library(neuralnet)
library(dummies)
library(scales)
car.df <- read.csv("ToyotaCorolla.csv", header = TRUE)
summary(car.df)
dim(car.df)
#preprocessing
#find an outlier in CC 
ccoutlier <- car.df[car.df$CC==16000,]
#remove this invalid outlier
car.df<- car.df[car.df$CC!=16000,]


#select numerical variable as in MlR class
numerical.var <- c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)
car2.df <- car.df[, numerical.var]
summary(car2.df)

#transform skewed variables before scale
hist(car2.df$KM)
#install.packages("e1071")
library(e1071)
skewness(car2.df$KM)
#since the skewness is greater than 1, we need to use logrithm to transform KM
car2.df$KM<-rescale(log(car2.df$KM)) #Rescales the Bedrooms column to 0-1
#rescale other predictors
car2.df$Age_08_04<-rescale(car2.df$Age_08_04)
car2.df$HP<-rescale(car2.df$HP)
car2.df$Met_Color<-rescale(car2.df$Met_Color)
car2.df$CC<-rescale(car2.df$CC)
car2.df$Doors<-rescale(car2.df$Doors)
car2.df$Quarterly_Tax<-rescale(car2.df$Quarterly_Tax)
car2.df$Weight<-rescale(car2.df$Weight)
summary(car2.df)
#Create dummies for Automatic
car2.df$Automatic<-as.factor(car2.df$Automatic)
car2.df<- dummy.data.frame(car2.df,sep=".")
summary(car2.df)
#partition data
set.seed(1) 
train.rows <- sample(rownames(car2.df), dim(car2.df)[1]*0.6)
cartrain.df <- car2.df[train.rows, ]
valid.rows <- setdiff(rownames(car2.df), train.rows) 
carvalid.df <- car2.df[valid.rows, ]

dim(cartrain.df)
dim(carvalid.df)
######################################################################
#neural network 
carnn <- neuralnet(Price ~ Age_08_04+KM+HP+Met_Color+Automatic.0+CC+Doors+Quarterly_Tax+Weight, data = cartrain.df, linear.output = T, hidden = 2)
# display weights
carnn$weights

# display predictions
prediction(carnn)

# plot network
plot(carnn, rep="best")

library(caret)
Pricepredict <- compute(carnn, data.frame(carvalid.df$Age_08_04, 
                                          carvalid.df$KM, 
                                          carvalid.df$HP, 
                                          carvalid.df$Met_Color,
                                          carvalid.df$Automatic.0, 
                                          carvalid.df$CC, 
                                          carvalid.df$Doors,
                                          carvalid.df$Quarterly_Tax, 
                                          carvalid.df$Weight))
Pricepredict 

#
valid.res <- data.frame(carvalid.df$Price, Pricepredict, residuals = 
                          carvalid.df$Price - Pricepredict)

library(forecast)
# use predict() to make predictions on a new set. 
pred <- predict(carnn, newdata = carvalid.df)
valid.res <- data.frame(carvalid.df$Price, prep, residuals = 
                          carvalid.df$Price - prep)


