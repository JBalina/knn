normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

get_dist <- function(x,y) {
  distance <- 0
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
  count <- 0
  tim <- 1
  for (valIndex in 1:nrow(val.n)) {
    #print(val.n[valIndex,])
    predicted <- 2
    count_2 <- 0
    count_4 <- 0
    #print(val.n[valIndex,])
    distances <- get_neighbors(val.n[valIndex,],train.n)
    i <- 1
    while (i < k) {
      #print(i)
      #print(distances[[i]][[2]][[length(distances[[i]][[2]])]])
      if (distances[[i]][[2]][[length(distances[[i]][[2]])]] == 2) {
        count_2 <- count_2+1/(distances[[i]][[1]][[1]]+1)
        #print("c2")
      } else {
        count_4 <- count_4+1/(distances[[i]][[1]][[1]]+1)
        #print("c4")
      }
      i <- i+1
    }
    #print(2)
    #print(count_2)
    #print(count_4)
    if(count_2 >= count_4) {
      predicted <- 2
    } else {
      predicted <- 4
    }
    #print(val.n[valIndex,])
    #print(predicted)
    if(predicted == val.n[valIndex,ncol(val.n)]) {
      count <- count+1
    } else {
      #print("nope")
    }
    #print(count)
    #print("ugh")
    #print(tim)
    tim <- tim + 1
    
  }
  #print("here I am")
  #print("why tho")
  #print(count)
  #print(nrow(val.n))
  print(count/nrow(val.n))
  #print("there i was")
  accuracy <- append(accuracy, (count/nrow(val.n)))
  
}
i <- 1
highestVal <- 0
highestInd <- 1
lowestVal <- 10
lowestInd <- 1
for (i in 1:length(accuracy)) {
  if(accuracy[[i]] > highestVal) {
    highestVal <- accuracy[i]
    highestInd <- i
  }
  if(accuracy[[i]] < lowestVal) {
    lowestVal <- accuracy[i]
    lowestInd <- i
  }
}

highK <- highestInd
lowK <- lowestInd
k <- lowK+(highK-lowK)/2
print(k)

for (testIndex in 1:nrow(test.n)) {
  #print(test.n[testIndex,])
  predicted <- 2
  count_2 <- 0
  count_4 <- 0
  #print(test.n[testIndex,])
  distances <- get_neighbors(test.n[testIndex,],train.n)
  i <- 1
  while (i < k) {
    #print(i)
    #print(distances[[i]][[2]][[length(distances[[i]][[2]])]])
    if (distances[[i]][[2]][[length(distances[[i]][[2]])]] == 2) {
      count_2 <- count_2+1/(distances[[i]][[1]][[1]]+1)
      #print("c2")
    } else {
      count_4 <- count_4+1/(distances[[i]][[1]][[1]]+1)
      #print("c4")
    }
    i <- i+1
  }
  #print(2)
  #print(count_2)
  #print(count_4)
  if(count_2 >= count_4) {
    predicted <- 2
  } else {
    predicted <- 4
  }
  #print(test.n[testIndex,])
  #print(predicted)
  if(predicted == test.n[testIndex,ncol(test.n)]) {
    count <- count+1
  } else {
    #print("nope")
  }
  #print(count)
  #print("ugh")
  #print(tim)
  tim <- tim + 1
  
}
print(count/nrow(test.n))

