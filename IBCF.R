
setwd("C:/Users/ashish_pc/Desktop/BTP mod")

#install.packages('recommenderlab')
library(recommenderlab)
library(reshape2)
library(ggplot2)

 
trd <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/ratings.data",sep="\t",header=F)
head(trd)

trd<- trd[,-c(4)]
names(trd) <- c("user","item","rating")
g<-acast(trd, user ~ item)

class(g)
dim(g)
testd <- read.csv("C:/Users/ashish_pc/Desktop/BTP/test.csv")
testd[,1] <- NULL

for(i in 1:20000)
{
  g[testd[i,1],testd[i,2]] <- NA
}
check <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/IBCF FIL.csv")
for(i in 1:16000)
{
  g[check[i,1],check[i,2]] <- NA
}
# Convert it as a matrix
R<-as.matrix(g)


r <- as(R, "realRatingMatrix")
rm(R,g)

image(r, main = "Raw Ratings")       

recd=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))

print(recd)
names(getModel(recd))

recomd <- predict(recd, r[1:nrow(r)], type="ratings")

recomd


test <- read.csv("C:/Users/ashish_pc/Desktop/movielens/test.csv")
head(test)
# Get ratings list
rec_list<-as(recomd,"list")
ratings<-NULL
# For all lines in test file, one by one
for ( k in 1:20000)
{
  
  userid <- test[k,2]
  movieid<-test[k,3]
  u1<-as.data.frame(rec_list[userid])
  
  u1$id<-row.names(u1)
  
  x= u1[u1$id==movieid,1]
  
  if (length(x)==0)
  {
    ratings[k] <- 0
  } else
  {
    ratings[k] <-x
  }
  ratings
}
pred <- ratings
summary(pred)
plot(pred)

#IBCF adjusting values <1
for(i in 1:20000)
{
if(pred[i] < 0) 
     {
pred[i] = mean(tr[tr$movie==test[i,2],3])
 }  
}
# Testing
out <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/BAGGING OUTPUT.csv")
summary(pred)
summary(out$REAL)
out$IBCF <- pred
write.csv(out,"C:/Users/ashish_pc/Desktop/BTP mod/BAGGING OUTPUT.csv",row.names=F)
library('Metrics')
rmse(out$REAL,pred)
mae(out$REAL,pred)
mse(out$REAL,pred)