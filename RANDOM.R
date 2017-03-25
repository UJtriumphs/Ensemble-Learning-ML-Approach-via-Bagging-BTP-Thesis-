
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
write.csv(g,"C:/Users/ashish_pc/Desktop/BTP/dummy10.csv",row.names=F)
testd <- read.csv("C:/Users/ashish_pc/Desktop/BTP/test.csv")
testd[,1] <- NULL

for(i in 1:20000)
{
  g[testd[i,1],testd[i,2]] <- NA
}
check <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/POP FIL.csv")
for(i in 1:16000)
{
  g[check[i,1],check[i,2]] <- NA
}

R<-as.matrix(g)
r <- as(R, "realRatingMatrix")
rm(R,g)

image(r, main = "Raw Ratings")       

recd=Recommender(r[1:nrow(r)],method="RANDOM",param=list(minrating=1))
print(recd)
names(getModel(recd))

recomd <- predict(recd, r[1:nrow(r)], type="ratings")

recomd


test <- read.csv("C:/Users/ashish_pc/Desktop/movielens/test.csv")
head(test)

rec_list<-as(recomd,"list")
ratings<-NULL

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

# Testing
out <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/BAGGING OUTPUT.csv")
summary(pred)
summary(out$REAL)
out$RANDOM <- pred
write.csv(out,"C:/Users/ashish_pc/Desktop/BTP mod/BAGGING OUTPUT.csv",row.names=F)
library('Metrics')
rmse(out$REAL,pred)
mae(out$REAL,pred)
mse(out$REAL,pred)