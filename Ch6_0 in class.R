###Some examples in SLR
housing1.df <- read.csv("BostonHousing.csv")
summary(housing1.df)

slr <- lm(MEDV ~ AGE, data=housing.df)
summary(slr)

housing2.df <- read.csv("WestRoxbury.csv", header = TRUE) 
summary(housing2.df)

slr2 <- lm(TOTAL.VALUE ~ LOT.SQFT, data=housing.df)
summary(slr2)

slr3 <- lm(MEDV ~ CHAS, data=housing1.df)
summary(slr3)

data.for.plot <- aggregate(housing1.df$MEDV, by = list(housing1.df$CHAS), FUN = mean)
data.for.plot #the column have no names
names(data.for.plot) <- c("CHAS", "MeanMEDV") #give names to columns

slr4 <- lm(TOTAL.VALUE ~ REMODEL, data=housing.df)
summary(slr4)

data.for.plot <- aggregate(housing2.df$TOTAL.VALUE, by = list(housing2.df$REMODEL), FUN = mean)
data.for.plot #the column have no names
names(data.for.plot) <- c("REMODEL", "Mean total Value") #give names to columns

##################################################################################################################
##Load the csv file
car.df <- read.csv("ToyotaCorolla.csv")
dim(car.df)
summary(car.df)
names(car.df)
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
numerical.var <- c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)

#price <- car.df$Price
#summary(price)
car2.df <- car.df[, numerical.var]


# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(car2.df, mean), 
           sd=sapply(car2.df, sd), 
           min=sapply(car2.df, min), 
           max=sapply(car2.df, max), 
           median=sapply(car2.df, median), 
           length=sapply(car2.df, length),
           miss.val=sapply(car2.df, function(x) 
             sum(length(which(is.na(x))))))


round(cor(car2.df),2)

table(car.df$Fuel_Type)

# create 10 bins for Age
car.df$Age_08_04.bin <- .bincode(car.df$Age_08_04, c(0,10,20,30,40,50,60,70,80))

c(0:8)*10

aggregate(car.df$Price, by=list(Age=car.df$Age_08_04.bin), FUN=mean)

# compute the average of Price by (binned) Age and Fuel Type
# in aggregate() use the argument by= to define the list of aggregating variables, 
# and FUN= as an aggregating function.
aggregate(car.df$Price, by=list(Age=car.df$Age_08_04.bin, 
                                          Fuel.Type=car.df$Fuel_Type), FUN=mean) 

####Creating Pivot Tables using melt and cast
library(reshape) 

# use melt() to stack a set of columns into a single column of data.
# stack Price values for each combination of (binned) Age and Fuel Type
mlt <- melt(car.df, id=c("Age_08_04.bin", "Fuel_Type"), measure=c("Price"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, Age_08_04.bin ~ Fuel_Type, subset=variable=="Price", 
     margins=c("grand_row", "grand_col"), mean)



# partition data

set.seed(1) ##Very important when running different methods with the same dataset

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as validation
train.rows <- sample(rownames(car2.df), dim(car2.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- car2.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(car2.df), train.rows) 
valid.data <- car2.df[valid.rows, ]

dim(train.data)
dim(valid.data)

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.

mvr <- lm(Price ~ ., data=train.data)

#  use options() to ensure numbers are not displayed in scientific notation.
#Summarize using summary()
options(scipen=999) # avoid scientific notation
summary(mvr)

 ######

library(forecast)
# use predict() to make predictions on a new set. 
pred <- predict(mvr, newdata = valid.data)
valid.res <- data.frame(valid.data$Price, pred, residuals = 
                       valid.data$Price - pred)
head(valid.res)

hist(valid.res$residuals)
sd(valid.res$residuals)
mean(valid.res$residuals)
length(valid.res$residuals[which(valid.res$residuals>3000 | valid.res$residuals< -3000)])
length(valid.res$residuals[which(valid.res$residuals>3000)])
length(valid.res$residuals[which(valid.res$residuals< -3000)])
length(valid.res$residuals[which(valid.res$residuals< 3000 & valid.res$residuals> -3000)])/400
length(valid.res$residuals[which(valid.res$residuals< 3000 & valid.res$residuals> -3000)])/length(valid.res$residuals)




# use accuracy() to compute common accuracy measures. 
#"test set" in the output means the data that lm analyzed, not "test set" in the data mining sense

accuracy(mvr$fitted.values, train.data$Price)

# compute accuracy on prediction set
accuracy(pred, valid.data$Price)



#######






###########

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually. May not be needed in newer versions
library(leaps)
search <- regsubsets(Price ~ ., data = train.data, nbest = 1, nvmax = dim(train.data)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq #Bad criteria to use. R-square always increases with number of variables
sum$adjr2
sum$cp

##########
# use step() to run stepwise regression.
car.lm.step <- step(mvr, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Price)

###########
# create model with no predictors
car.lm.null <- lm(Price~1, data = train.data)
# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=mvr), direction = "forward")
summary(car.lm.step)  # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Price)

############
# use step() to run stepwise regression.
car.lm.step <- step(mvr, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Price)
