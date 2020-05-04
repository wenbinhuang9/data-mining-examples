
setwd("/Users/ben/Wenbin_GitHub/data-mining-examples/data")

library(neuralnet)
library(dummies)
library(scales)
car.df <- read.csv("ToyotaCorolla.csv", header = TRUE)
summary(car.df)
dim(car.df)
#preprocessing
#find an outlier in CC 
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

hist(car2.df$Price)
skewness(car2.df$Price)
car2.df['Price.Log'] <- log(car2.df$Price)
car2.df$Price.Log
car2.df['Price.Scale']<-rescale(log(car2.df$Price.Log))

skewness(car2.df$Price.Scale)

#partition data
set.seed(1) 
train.rows <- sample(rownames(car2.df), dim(car2.df)[1]*0.6)
cartrain.df <- car2.df[train.rows, ]
valid.rows <- setdiff(rownames(car2.df), train.rows) 
carvalid.df <- car2.df[valid.rows, ]

dim(cartrain.df)
dim(carvalid.df)


head(cartrain.df)
head(carvalid.df)
car_nn <- neuralnet(Price.Scale~KM+Age_08_04+Met_Color+CC+Doors+Weight+Automatic.0+Quarterly_Tax+HP,
                    data=cartrain.df, 
                    hidden = 6,
                    linear.output = FALSE)

plot(car_nn)
library(caret)
predict <- compute(car_nn, data.frame(carvalid.df$KM,
                                      carvalid.df$Age_08_04,
                                      carvalid.df$Met_Color,
                                      carvalid.df$CC,
                                      carvalid.df$Doors,
                                      carvalid.df$Weight,
                                      carvalid.df$Automatic.0,
                                      carvalid.df$Quarterly_Tax,
                                      carvalid.df$HP))
predict

max.price.in.val <- max(carvalid.df$Price.Log)
min.price.in.val <- min(carvalid.df$Price.Log)
print(max.price.in.val)
print(min.price.in.val)

class(predict$net.result)

unscale.fun <-function(x) {
  x * (max.price.in.val - min.price.in.val) + min.price.in.val
}

predict.unscale <- apply(predict$net.result, 2, unscale.fun)
predict.unscale

predict.recover <- apply(predict.unscale, 2, exp)
predict.recover

predict.df <- data.frame(predict.recover)

diff.df <- data.frame(carvalid.df$Price, predict.df$predict.recover, 
                      residual = carvalid.df$Price - predict.df$predict.recover)
diff.df

library(forecast)
accuracy(predict.df$predict.recover, carvalid.df$Price)


