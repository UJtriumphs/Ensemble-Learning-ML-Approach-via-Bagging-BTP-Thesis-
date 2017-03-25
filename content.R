
# Genre based Recommender Engine
# Movie-Movie Similarity Matrix using Jaccard Coefficient
# Jaccard Function
JACCARD <- function(x,y)
{
  ln <- length(x)
  sim_1 <- 0
  sim_2 <- 0
  for(i in 1:ln)
  {
    if(x[i]==1 & y[i]==1) {
      sim_1 <- sim_1 + 1
    }
    if(x[i]==0 & y[i]==0) {
      sim_2 <- sim_2 + 1
    }
  }
  sim_1/(ln-sim_2)
}


# Setting up Directory
setwd("F:\BTP\BTP MOD OLD")
input <- read.csv("movie.item",sep="|", header = F)
# str(input)
input$V2 <- input$V3 <- input$V4 <- input$V5 <- NULL 
summary(input)
names(input)[1] <- "movie id"

# Removing movie id as it is redundant
input[,1] <- NULL

# Writing Genres of movies onto file "genres.csv"
write.table(input,"genres.csv",row.names= F, col.names=F, sep=",")



# Creating movie-movie matrix
sz <- nrow(input)
movie_sim_mat <- matrix(, nrow = sz, ncol = sz)
for(i in 1:sz)
{
  #print(i)
  for(j in 1:sz)
  {
    if(i == j)  movie_sim_mat[i,j] <- 1
    else 
      movie_sim_mat[i,j] <- movie_sim_mat[j,i] <- JACCARD(input[i,], input[j,])
  }
}

write.csv(movie_sim_mat,"movie-similarity.csv", row.names= F)

library('reshape2')

# Training Phase
# Train with Random Created Dataset 80% of Training Dataset

train <- read.csv("F:/BTP  mod/trainrandom_2.csv")
train[,1] <- NULL
rat <- read.delim("F:/BTP  mod/ratings.data", header=FALSE)
rat[,4] <- NULL
names(rat) <- c("user","item","rating")

# Casted array acoording to our requirements
m <- dcast(rat, user ~ item, value.var = 'rating')
m[,1] <- NULL


testing <- read.csv("F:/BTP  mod/test.csv")
testing[,1] <- NULL
for(i in 1:20000)
{
  m[testing[i,1],testing[i,2]] <- NA
}

# ind to store number of movies watched by single user
# watch to store the movies watched by the user
ind  <- vector(mode="numeric", length=943)
watch <- matrix(, nrow=943,ncol=600)

for(i in 1:943)
{
  for(j in 1:1682)
  {
    if(!is.na(m[i,j]))
    {
      ind[i] <- ind[i] + 1
      watch[i,ind] <- j 
    }
  }
}
watch[1,]

# Testing Phase

prediction <- vector(mode="numeric",length=20000)
for(i in 1:20000)
{
  u <- testing[i,1]
  movie_id <- testing[i,2]
  prediction[i] <- sum(movie_sim_mat[movie_id,watch[u,1:ind[u]]]*m[u,watch[u,1:ind[u]]])/sum(movie_sim_mat[movie_id,watch[u,1:ind[u]]])
  if(is.nan(prediction[i]))
    prediction[i] <- mean(m[,movie_id], na.rm=T)
  if(is.nan(prediction[i]))
    prediction[i] <- mean(as.numeric(m[u,]),na.rm=T)
}

# Analysis and Output Phase
library('Metrics')
rmse(prediction[1:20000],testing[1:20000,3])

output <- read.csv("F:/BTP  mod/output1level_1.csv")
output$"content" <- prediction
output <- output[c(1,2,3,4,6,5)]
write.csv(output,"output1level_1.csv",row.names=F)


rmse(output[,5],output[,6])
mse(output[,5],output[,6])
mae(output[,5],output[,6])
save.image()

summary(prediction)
