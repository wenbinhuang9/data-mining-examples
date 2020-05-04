######

#Load utilities data
utilities.df <- read.csv(file="utilities.csv", header = TRUE)
summary(utilities.df)

#Do a scatter plot Fuel Cost vs. Sales
plot(utilities.df$Fuel_Cost ~ utilities.df$Sales, xlab ="Sales", ylab = "Fuel_Cost")
text(utilities.df$Sales+0.1, utilities.df$Fuel_Cost+0.1, labels = utilities.df$Company)

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
summary(utilities.df)

utilities1.df <- utilities.df[,-1]
summary(utilities1.df)
row.names(utilities1.df)

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
utild <- dist(utilities1.df, method = "euclidean")
utild

####

# normalize input variables. 
library(caret)
util.norm <- preProcess(utilities1.df, method=c("center", "scale"))
util.norm.df <- predict(util.norm, utilities1.df)

summary(util.norm.df)

# Label rows for your normalized data
row.names(util.norm.df)

# compute normalized distance based on variables Sales and FuelCost
utildnorm1 <- dist(util.norm.df[,c(6,8)], method = "euclidean")
utildnorm1

#### 
# compute normalized distance based on all 8 variables
utildnorm <- dist(util.norm.df[,], method = "euclidean")
utildnorm

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
#hang indicates the fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0.
#ann 	a logical value indicating whether the default annotation (title and x and y axis labels) should appear on the plot.
Utilhc1 <- hclust(utildnorm, method = "single")
plot(Utilhc1, hang = -1, ann = FALSE)

#Use some other method now

Utilhc2 <- hclust(utildnorm, method = "complete")
plot(Utilhc2, hang = -1, ann = FALSE)



#######
#Get membership numbers

Utilmemb <- cutree(Utilhc1, k = 6)

Utilmemb



###########

# set labels as cluster membership and utility name
row.names(util.norm.df) <- paste(Utilmemb, ": ", row.names(utilities.df), sep = "")

row.names(utilities1.df) <- paste(Utilmemb, ": ", row.names(utilities.df), sep = "")


# plot heatmap 
# nice reference http://stanstrup.github.io/heatmaps.html
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(util.norm.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

heatmap(as.matrix(utilities1.df), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))



#######kmeans

# load and preprocess data 
utilities.df <- read.csv(file="utilities.csv", header = TRUE)
summary(utilities.df)

#Do a scatter plot Fuel Cost vs. Sales
plot(utilities.df$Fuel_Cost ~ utilities.df$Sales, xlab ="Sales", ylab = "Fuel_Cost")
text(utilities.df$Sales+0.1, utilities.df$Fuel_Cost+0.1, labels = utilities.df$Company)

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
summary(utilities.df)

utilities1.df <- utilities.df[,-1]
summary(utilities1.df)
row.names(utilities1.df)


# normalized distance:

util.norm <- preProcess(utilities1.df, method=c("center", "scale"))
util.norm.df <- predict(util.norm, utilities1.df)

summary(util.norm.df)

# Label rows for your normalized data
row.names(util.norm.df) <- row.names(utilities.df)
row.names(utilities.df)

# run kmeans algorithm 
set.seed(2)
Utilkm <- kmeans(util.norm.df, 6)

# show cluster membership
Utilkm$cluster



#### 
# centroids
Utilkm$centers



#### 

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(Utilkm$centers), max(Utilkm$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities1.df))

# plot centroids
for (i in c(1:6)){
  lines(Utilkm$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))}

# name clusters
text(x = 0.5, y = Utilkm$centers[, 1], labels = paste("Cluster", c(1:6)))



#### 

dist(Utilkm$centers)

Utilkm
Utilkm$withinss
sum(Utilkm$withinss)
mean(Utilkm$withinss)

Utilkm1 <- kmeans(util.norm.df, 1)
Utilkm1$withinss

##Effect of varying k (analyze similar to what we did in knn).. 
#This time the measure can be average within SS rather than accuracy

UtilkvsWithinSS.df <- data.frame(k = seq(1, 10, 1), AvgWithinSS = rep(0, 10)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
UtilkvsWithinSS.df

# compute AvgWithinSS for different k
for(i in 1:10) {
  UtilkvsWithinSS.df[i,2] <-  mean(kmeans(util.norm.df, i)$withinss)
}

plot(UtilkvsWithinSS.df$AvgWithinSS ~ UtilkvsWithinSS.df$k, xlab="k", ylab = "Average WithinSS", lty=2, type="l")
