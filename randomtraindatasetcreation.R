tr <- NULL
tr <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/train.csv",header=F)
user <- NULL
item <- NULL
rating <- NULL
p <- NULL
p <- sample(1:80000,16000,replace=FALSE)
head(p)
for(u in 1:16000)
{
  
  user[u] <- tr[p[u],2]
  item[u] <- tr[p[u],3]
  rating[u] <- tr[p[u],4]
}

out <-cbind(user,item,rating)
head(out)
write.csv(out,"C:/Users/ashish_pc/Desktop/BTP mod/UBCF2 FIL.csv",row.names=F)