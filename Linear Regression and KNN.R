#change working directory to desktop
setwd("C:/Users/NelsonJ/Desktop") 

#read csv file
Data <- read.csv("day.csv")

#We will be regressing on the cnt variable using everything but registered and casual

#2b
#Setup the data into training and testing sets
numrow <- nrow(Data)
train <- sample(1:numrow, floor(2/3 * numrow), replace = F)
val <- setdiff(1:numrow, train)

#2c
#Fit linear model
summary(lm(Data[train,16] ~ ., Data[train,c(3:13)]))

#Graph of regression line and others
par(mfrow=c(2,2))
plot(lm(Data[train,16] ~ ., Data[train,c(3:13)]))
par(mfrow=c(1,1))

#2d

InterTermTempHum = Data$temp * Data$hum

#http://en.wikipedia.org/wiki/Atmospheric_temperature

InterTempTempaTemp = Data$temp * Data$atemp

InterTermTempWind = Data$temp * Data$windspeed

DataNew = Data[,c(3:13)]
DataNew = cbind(DataNew, InterTermTempHum,
                InterTempTempaTemp,InterTermTempWind)

summary(lm(Data[train,16] ~ ., DataNew[train,]))

par(mfrow=c(2,2))
plot(lm(Data[train,16] ~ ., DataNew[train,]))
par(mfrow=c(1,1))

#2e/f(Analysis of predictors version)
#http://www.cs.umb.edu/~dsim/papersps/simovici-Feature1.pdf
#http://stackoverflow.com/questions/16249625/difference-between-pca-principal-component-analysis-and-feature-selection
#http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
#http://jmlr.csail.mit.edu/papers/volume3/guyon03a/guyon03a.pdf

cl <- kmeans(DataNew[train,], 14)

plot(DataNew[train,], col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

#Maybe try this plotting method http://www.r-bloggers.com/bot-botany-k-means-and-ggplot2/

par(mar = c(5.1, 4.1, 0, 1))
plot(DataNew[train,],
  col = cl$cluster,
  pch = '.', cex = 5)
  points(cl$centers, pch = 4, cex = 4, lwd = 4)

#After evaluating the graphs it seems that month,holiday,workingday can be removed.
#Will go more indepth in the write up in regards to feature choices.

DataRemoved = DataNew[-c(3,4,6)]

summary(lm(Data[train,16] ~ ., DataRemoved[train,]))

#2e/f(PCA Version)
#Using prcomp from the stats package

data.pca <- prcomp(DataNew[train,], center = TRUE, scale = TRUE)
print(data.pca)
plot(data.pca, type = "l")
summary(data.pca)

#Shows variance of each PC
plot(data.pca, xlab = "principle components")

#http://staffhome.ecm.uwa.edu.au/~00013289/3S6/Lectures/PCA.pdf
#Indicates the difference in compenents, arrows that point in pos direction vs neg direction
#X-axis represents 32% of variance while y-axis represents 16%.
biplot(data.pca, col = c("white", "red"))
biplot(data.pca, col = c("black", "red"))

plot(Data[train,]$cnt, data.pca$x[,1])

data.pca$x[, 1]

summary(lm(Data[train,16] ~ data.pca$x[, 1] + data.pca$x[, 2] + data.pca$x[, 3] + data.pca$x[, 4]))

#2g

freqparcoord(Data[train,],5,3:13,method="maxdens")

train_set <- smoothz(Data[train,c(3:13, 16)], sf=knnreg,k=5)

count <- Data[train,16]

u <- ggplot(data.frame(count, train_set))
u + geom_smooth(aes(x = count, y = train_set))

val_set <- smoothzpred(Data[val,c(3:13)], Data[train,c(3:13)], train_set)
val_count <- Data[val,16]

k <- ggplot(data.frame(val_count, val_set))
k + geom_smooth(aes(x = val_count, y = val_set))

#2h
