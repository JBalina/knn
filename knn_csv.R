sink("knnResults.txt")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

get_dist <- function(x,y) {
  distance <- 0
  #print(y)
  #print(x)
  for (i in 1:(length(x)-1)) {
    #print(x)
    #print(x[i])
    h <- (x[[i]] - y[[i]])^2
    #print(h[[1]])
    distance <- (distance+h[[1]])
  }
  distance <- sqrt(distance)
  return(list(distance,y))
}

get_neighbors <- function(l1,l2) {
  #print(l1)
  distances <- list()
  #print(nrow(l2))
  for (i in 1:nrow(l2)) {
    #print(l2[i,])
    dist <- get_dist(l1,l2[i,])
    #print(dist)
    #Sys.sleep(2)
    distances[[length(distances)+1]] <- dist
    #print(distances)
    #Sys.sleep(3)
  }
  #print(distances)
  return (distances[order(sapply(distances, function(x) x[[1]][[1]]))])
}

get_acc <- function(vList, k, count) {
  #print(val.n[valIndex,])
  predicted <- 2
  count_2 <- 0
  count_4 <- 0
  #print(val.n[valIndex,])
  distances <- apply(train.n, 1, get_dist,x=vList)
  distances <- distances[order(sapply(distances, function(x) x[[1]][[1]]))]
  i <- 1
  while (i < k) {
    if (distances[[i]][[2]][[length(distances[[i]][[2]])]] == 2) {
      count_2 <- count_2+1/(distances[[i]][[1]][[1]]+1)
      #print("c2")
    } else {
      count_4 <- count_4+1/(distances[[i]][[1]][[1]]+1)
      #print("c4")
    }
    i <- i+1
  }
  if(count_2 >= count_4) {
    predicted <- 2
  } else {
    predicted <- 4
  }
  if(predicted == vList[length(vList)]) {
    count <- count+1
  } else {
    #print("nope")
  }
  #tim <- tim + 1
  return(count)
}

test <- read.csv("test.csv", header = FALSE)
test.n <- as.data.frame(lapply(test[,1:10], normalize))
test.n$v11 <- test[,11]
test.n$V1 <- NULL
#test.n.list <- split(test.n, seq(nrow(test.n)))
train <- read.csv("train.csv", header = FALSE)
train.n <- as.data.frame(lapply(train[,1:10], normalize))
train.n$v11 <- train[,11]
train.n$V1 <- NULL
#train.n.list <- split(train.n, seq(nrow(train.n)))
val <- read.csv("val.csv", header = FALSE)
val.n <- as.data.frame(lapply(val[,1:10], normalize))
val.n$v11 <- val[,11]
val.n$V1 <- NULL
#val.n.list <- split(val.n, seq(nrow(val.n)))


#print(test.n.list)
accuracy <- list()

for(k in 1:nrow(train.n)) {
  count <- apply(val.n, 1, get_acc,k=k,count = 0)
  print(sum(count)/nrow(val.n))
  accuracy <- append(accuracy, (sum(count)/nrow(val.n)))
  
}
i <- 1
highestVal <- 0
highestInd <- 1
highLen <- 1

for (i in 1:length(accuracy)) {
  if(accuracy[[i]] > highestVal) {
    highestVal <- accuracy[[i]]
    highestInd <- i
  }
  if(accuracy[[i]] == accuracy[[highestInd]]) {
    highLen <- i
  }
}

highK <- highestInd
#highLen <- lowestInd
#print(highK)
#print(highLen)
k <- highK+(highLen-highK)/2
print(k)


count <- apply(test.n, 1, get_acc,k=k,count = 0)
print(sum(count)/nrow(test.n))

sink()