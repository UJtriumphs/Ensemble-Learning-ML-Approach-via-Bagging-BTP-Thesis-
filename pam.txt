# set working directory 
setwd("C:/Users/ashish_pc/Desktop/BTP mod")
# real is backup copy of data to rollback bad operations
user <- read.csv('user.user',header=F,sep='|')
# removed user-id as it is same as row number
user[,1] <- NULL
str(user)
summary(user)
# viewing summary we see that no NA's ... cool
names(user) <- c('age','gender','occupation','zipcode')
write.csv(user,'user-clusteringp.csv',row.names=F)
# encoding M = 1 , F = 0 
user$gender <- as.numeric(user$gender) - 1

norm <- function(x) { (x - mean(x))/sd(x) }
# normalize attributes
user$age <- norm(user$age)
user$gender <- norm(user$gender)
user$zipcode <- as.numeric(user$zipcode)
user$zipcode <- norm(user$zipcode)
write.csv(user,'user-clusteringp.csv',row.names=F)

library('cluster')
# daisy - calculate distance matrix for mixed data(categorical+nominal)
# pam - partitioning around mediods
silb <- vector(,100)
set.seed(1000)
dia <- vector(,100)
sil <- vector(,100)
for(i in 21:24)
{
  clus <- pam(daisy(user, metric="gower",stand= TRUE), k=37,stand=T )
  #dia[i] <- mean(clus$clusinfo[,4])
  sil[i] <- (clus$silinfo)[3][[1]]
}
#plot(dia[25:43],type='o',col='red')
plot(sil[2:43],type='o',col='red')
clus
clusplot(user, clus$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

silb[21:24] <- sil[21:24]
silb[2:43]
plot(2:43,silb[2:43],type='o',col='red',xlab="Cluster Size",ylab="Average Silhouette",
     main = "Silhouette-optimal number of Clusters")
which.max(silb) # hence number if clusters should be 37
max(silb) # 0.50865 A reasonable structure has been found

clus <- pam(daisy(user, metric="gower",stand= TRUE), k=37,stand=T )


# Allocating cluster number to training data
train <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/PAM FIL.csv")
names(train)
#train$X <- NULL
train$cluster <- clus$cluster[train$user]
write.csv(train,"train-clusterp.csv",row.names=F)

# Prediction
test <- read.csv("C:/Users/ashish_pc/Desktop/BTP/test.csv")
names(test)
test$X <- NULL

pred <- vector(,nrow(test))

for(i in 1:20000)
{
  c <- clus$cluster[test$user[i]]
  it <- test$item[i]
  
  pred[i] <-  mean(train[train$cluster==c & train$item==it,]$rating)
  if(is.na(pred[i]) | is.na(pred[i]))
  {
    pred[i] <- mean(train[train$item==it,]$rating)
  }
  if(is.na(pred[i]) | is.na(pred[i]))
  {
    pred[i] <- mean(train[train$user==test$user[i],]$rating)
  }
}

# Testing
out <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/BAGGING OUTPUT.csv")
summary(out$PAM)
summary(pred)
out$PAM <- pred
write.csv(out,"BAGGING OUTPUT.csv",row.names=F)

library('Metrics')
rmse(out$REAL,pred)
mae(out$REAL,pred)
mse(out$REAL,pred)
# http://www.stat.berkeley.edu/~s133/Cluster2a.html






