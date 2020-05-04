#Q2.ADevelop a neural network to predict Price. You may use the same predictors we had
#used in class (while discussing multivariable regression). Start with one hidden layer
#with 2 nodes. Make sure you scale the data.

setwd("/Users/ben/Wenbin_GitHub/data-mining-examples/data")
library(neuralnet)
library(dummies)
library(scales)
car.df <- read.csv("ToyotaCorolla.csv", header = TRUE)
summary(car.df)
dim(car.df)

############################################################ preprocessing
#remove this invalid outlier
car.df<- car.df[car.df$CC!=16000,]


#select numerical variable as in MlR class
numerical.var <- c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)
car2.df <- car.df[, numerical.var]
summary(car2.df)

#partition data
set.seed(1) 
train.rows <- sample(rownames(car2.df), dim(car2.df)[1]*0.6)
cartrain.df <- car2.df[train.rows, ]
valid.rows <- setdiff(rownames(car2.df), train.rows) 
carvalid.df <- car2.df[valid.rows, ]

dim(cartrain.df)
dim(carvalid.df)

#rescale train data
#transform skewed variables before scale
hist(cartrain.df$Price)
#install.packages("e1071")
library(e1071)
skewness(cartrain.df$Price)
#since the skewness is greater than 1, we need to use logrithm to transform Price
cartrain.df$Price_log <- log(cartrain.df$Price)
cartrain.df$Price_r <- rescale(cartrain.df$Price_log)
#rescale other predictors 
cartrain.df$KM <- rescale(cartrain.df$KM) 
cartrain.df$Age_08_04<-rescale(cartrain.df$Age_08_04)
cartrain.df$HP<-rescale(cartrain.df$HP)
cartrain.df$Met_Color<-rescale(cartrain.df$Met_Color)
cartrain.df$CC<-rescale(cartrain.df$CC)
cartrain.df$Doors<-rescale(cartrain.df$Doors)
cartrain.df$Quarterly_Tax<-rescale(cartrain.df$Quarterly_Tax)
cartrain.df$Weight<-rescale(cartrain.df$Weight)
summary(cartrain.df)

#rescale valid data
carvalid.df$Price_log <- log(carvalid.df$Price)
carvalid.df$Price_r <- rescale(carvalid.df$Price_log)
#rescale other predictors 
carvalid.df$KM <- rescale(carvalid.df$KM) 
carvalid.df$Age_08_04<-rescale(carvalid.df$Age_08_04)
carvalid.df$HP<-rescale(carvalid.df$HP)
carvalid.df$Met_Color<-rescale(carvalid.df$Met_Color)
carvalid.df$CC<-rescale(carvalid.df$CC)
carvalid.df$Doors<-rescale(carvalid.df$Doors)
carvalid.df$Quarterly_Tax<-rescale(carvalid.df$Quarterly_Tax)
carvalid.df$Weight<-rescale(carvalid.df$Weight)
summary(carvalid.df)
############################################################## neural network
#run neural network 
carnn <- neuralnet(Price_r ~ Age_08_04 + KM + HP + Met_Color + CC + Doors + Quarterly_Tax + Weight + Automatic, 
                   data = cartrain.df, linear.output = F, hidden = 2)
# display weights
carnn$weights
# plot network
plot(carnn, rep="best")

# predict price on validation data
library(caret)
Pricepredict <- compute(carnn, data.frame(carvalid.df$Age_08_04, 
                                          carvalid.df$KM, 
                                          carvalid.df$HP, 
                                          carvalid.df$Met_Color,
                                          carvalid.df$Automatic, 
                                          carvalid.df$CC, 
                                          carvalid.df$Doors,
                                          carvalid.df$Quarterly_Tax, 
                                          carvalid.df$Weight))
Pricepredict 

#transform back to original Price units
unrescale<-function(x){
  x*(max(carvalid.df$Price_log)-min(carvalid.df$Price_log)) + min(carvalid.df$Price_log)
}

pricepred_unrescale<-apply(Pricepredict$net.result, 2, unrescale)
#unlog price
pricepred_unlog<-apply(pricepred_unrescale, 2 ,exp)
#put into daraframe
pricepred_unlog.df<-data.frame(pricepred_unlog)
head(pricepred_unlog.df$pricepred_unlog)
head(carvalid.df$Price)
#compute residuals
valid.res <- data.frame(Actual_price = carvalid.df$Price, 
                        predicted = pricepred_unlog, 
                        residuals = carvalid.df$Price - pricepred_unlog)
#show first 6 rows of residuals
head(valid.res)

#install.packages("Metrics")
library(Metrics)
print(rmse(carvalid.df$Price, pricepred_unlog.df$pricepred_unlog))

library(forecast)
accuracy(pricepred_unlog.df$pricepred_unlog, carvalid.df$Price)




