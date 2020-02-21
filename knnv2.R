normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

get_dist <- function(x,y) {
  distance <- 0
  for (i in 1:length(x)) {
    h <- (x[i] - y[i])^2
    distance <- (distance + h)
  }
  distance <- sqrt(distance)
  return(list(distance,y))
}

get_neighbors <- function(l1,l2) {
  distances <- list()
  for (trainSet in l2) {
    dist <- get_dist(l1,trainSet)
    #print(dist)
    distances[[length(distances)+1]] <- dist
    #print(distances)
    #Sys.sleep(3)
  }
  return (distances[order(sapply(distances, function(x) x[[1]][[1]]))])
}

data <- read.table("monks-1.train", header = FALSE)
data <- data[ ,1:7]
data.n <- as.data.frame(lapply(data[1:6], normalize))
data.n$v7 <- data[,7]
data.n.list <- split(data.n, seq(nrow(data.n)))
#print(data.n.list)
trainSize <- floor(0.8*nrow(data))
valSize <- nrow(data.n.list)-trainSize
data.n.list <- sample(data.n.list, length(data.n.list), replace = FALSE)
train.n.list <- data.n.list[1:trainSize]
val.n.list <- data.n.list[(trainSize+1):length(data.n.list)]
test <- read.table("monks-1.test", header = FALSE)
test <- test[ ,1:7]
test.n <- as.data.frame(lapply(test[1:6], normalize))
test.n$v7 <- test[,7]
test.n.list <- split(test.n, seq(nrow(test.n)))
#print(train.n.list)
#print(val.n.list)


#print(test.n.list)
accuracy <- list()

for(k in 1:length(train.n.list)) {
  count <- 0
  tim <- 1
  for (v3 in val.n.list) {
    predicted <- 2
    count_1 <- 0
    count_2 <- 0
    distances <- get_neighbors(v3,train.n.list)
    for (i in 1:k) {
      if (distances[[i]][[2]][[length(distances[[i]][[2]])]] == 1) {
        count_1 <- count_1+1/(distances[[i]][[1]]+1)
      }
      else {
        count_2 <- count_2+1/(distances[[i]][[1]]+1)
      }
    }
    if (count_1 >= count_2) {
      predicted <- 1
    }
    else {
      predicted <- 2
    }
    #print(v3[[length(v3)]])
    #print(predicted)
    if (predicted == v3[[length(v3)]]) {
      count <- count+1
    }
    #print(tim)
    tim <- tim + 1
    
  }
  #print("here I am")
  #print(count)
  #print(length(val.n.list))
  print(count/length(val.n.list))
  #print("there i was")
  accuracy <- append(accuracy, (count/length(val.n.list)))
  
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

for (v3 in test.n.list) {
  predicted <- 1
  count_1 <- 0
  count_2 <- 0
  distances <- get_neighbors(v3,train.n.list)
  for (i in 1:k) {
    if (distances[[i]][[2]][[length(distances[[i]][[2]])]] == 1) {
      count_1 <- count_1+1/(distances[[i]][[1]]+1)
    }
    else {
      count_2 <- count_2+1/(distances[[i]][[1]]+1)
    }
  }
  if (count_1 >= count_2) {
    predicted <- 1
  }
  else {
    predicted <- 2
  }
  if (predicted == v3[[length(v3)]]) {
    count <- count+1
  }
  tim <- tim + 1
  
}
print(count/length(test.n.list))

