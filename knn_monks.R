#Jonathan Balina
#weighted knn
#unweighted knn
#k-dtree

myMedian <- function(x) {
  medi <- 0
  if((length(x) %% 2) == 0) {
    medi <- (x[(length(x)/2)+1])
    i <- 2
    while (medi == x[1]) {
      medi <- x[i]
      i <- i+1
    }
    return(medi)
  } else {
    medi <- (median(x))
    i <- 2
    while (medi == x[1]) {
      medi <- x[i]
      i <- i+1
    }
    return(medi)
  }
    
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

get_dist <- function(x,y) {
  distance <- 0
  for (i in 2:(length(x))) {
    h <- (x[i] - y[i])^2
    distance <- (distance + h)
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
  count_0 <- 0
  count_1 <- 0
  #print(val.n[valIndex,])
  distances <- apply(train.n, 1, get_dist,x=vList)
  distances <- distances[order(sapply(distances, function(x) x[[1]][[1]]))]
  i <- 1
  while (i < k) {
    if (distances[[i]][[2]][[1]] == 0) {
      count_0 <- count_0+1/(distances[[i]][[1]][[1]]+1)
      #print("c2")
    } else {
      count_1 <- count_1+1/(distances[[i]][[1]][[1]]+1)
      #print("c4")
    }
    i <- i+1
  }
  if(count_0 >= count_1) {
    predicted <- 0
  } else {
    predicted <- 1
  }
  if(predicted == vList[1]) {
    count <- count+1
  } else {
    #print("nope")
  }
  #tim <- tim + 1
  return(count)
}

get_uw_acc <- function(vList, k, count) {
  #print(val.n[valIndex,])
  predicted <- 2
  count_0 <- 0
  count_1 <- 0
  #print(val.n[valIndex,])
  distances <- apply(train.n, 1, get_dist,x=vList)
  distances <- distances[order(sapply(distances, function(x) x[[1]][[1]]))]
  i <- 1
  while (i < k) {
    if (distances[[i]][[2]][[1]] == 0) {
      count_0 <- count_0+1
      #print("c2")
    } else {
      count_1 <- count_1+1
      #print("c4")
    }
    i <- i+1
  }
  if(count_0 >= count_1) {
    predicted <- 0
  } else {
    predicted <- 1
  }
  if(predicted == vList[1]) {
    count <- count+1
  } else {
    #print("nope")
  }
  #tim <- tim + 1
  return(count)
}

get_accKD <- function(vList, count) {
  #print(vList[[3]])
  predicted <- get_class(train.tree, vList)
  #predicted <- 2
  count_0 <- 0
  count_1 <- 0
  if(predicted == vList[1]) {
    count <- count+1
  }
  return(count)
}

get_class <- function(tree, list) {
  if (length(tree)>0 & is.numeric(tree)) {
    return(tree)
  } else if (list[[tree[[3]]]] >= tree[[4]]) {
    return(get_class(tree[[2]], list))
    #>=tree[4]
  } else {
    return(get_class(tree[[1]], list))
  }
}

makeTree <- function(df,count) {
  if(count > 7) {
    count <- 2
  }
  #print(length(unique(df[,count])))
  #print(apply(df[,count],2,function(x) length((x))))
  while ((length(unique(df[,count])) == 1) && (length(df[,count])) > 1) {
    #print(df)
    count <- count + 1
    #print(count)
    if(count > 7) {
      count <- 2
    }
  }
  #print(df)
  df <- df[order(df[,count]),]
  #print(df)
  if(!(0 %in% df[,1])) {
    #there are only 1s
    return(1)
  }
  else if(!(1 %in% df[,1])) {
    #only 0s
    return(0)
  }
  else {
    #print("or here")
    medi <- match(myMedian(df[,count]),df[,count])
    #print(df)
    #print(count)
    #print(medi)
    df1 <- df[1:(medi-1),]
    df2 <- df[medi:nrow(df),]
    return(list(makeTree(df1,(count+1)),makeTree(df2,(count+1)),count,myMedian(df[,count])))
  }
}

#set.seed(32)
data <- read.table("monks-1.train", header = FALSE)
data <- data[ ,1:7]
data.n <- cbind(class=data[,1],as.data.frame(lapply(data[2:7], normalize)))
#data.n$v7 <- data[,1]
data.n.list <- split(data.n, seq(nrow(data.n)))
#print(data.n)
trainSize <- floor(0.8*nrow(data))
valSize <- nrow(data.n.list)-trainSize
data.n <- data.n[sample(nrow(data.n), replace = FALSE),]
train.n <- data.n[1:trainSize,]
val.n <- data.n[(trainSize+1):nrow(data.n),]
test <- read.table("monks-1.test", header = FALSE)
test <- test[ ,1:7]
test.n <- cbind(class=test[,1],as.data.frame(lapply(test[2:7], normalize)))

###########################################################

train.tree <- makeTree(train.n, 2)
count <- apply(test.n, 1, get_accKD, count = 0)
sink("kdtreeResults.txt")
print("K-dtree accuracy:")
print(sum(count)/nrow(test.n))
sink()

############################################################

sink("weightedResults.txt")
print("weighted knn accuracy:")
accuracy <- list()
for(k in 1:nrow(train.n)) {
  count <- apply(val.n, 1, get_acc,k=k,count = 0)
  #print(k)
  print(sum(count)/nrow(val.n))
  accuracy <- append(accuracy, (sum(count)/nrow(val.n)))
  
}
i <- 1
highestVal <- 0
highestInd <- 1
highLen <- 1

for (i in 1:length(accuracy)) {
  if(accuracy[[i]] > highestVal) {
    #print(accuracy[[i]])
    #print(i)
    highestVal <- accuracy[[i]]
    highestInd <- i
    highLen <- i
  }
  if((accuracy[[i]] == accuracy[[highestInd]]) & ((i-highLen) <= 1)) {
    highLen <- i
    #print("highLen:")
    #print(i)
  }
}

highK <- highestInd
#highLen <- lowestInd
#print(highK)
#print(highLen)
k <- as.integer(highK+(highLen-highK)/2)
print("Best k:")
print(k)

print("Best k on test")
count <- apply(test.n, 1, get_acc,k=k,count = 0)
print(sum(count)/nrow(test.n))
sink()

################################################################

sink("unweightedResults.txt")
print("Unweighted knn accuracy:")
accuracy <- list()
for(k in 1:nrow(train.n)) {
  count <- apply(val.n, 1, get_uw_acc,k=k,count = 0)
  #print(k)
  print(sum(count)/nrow(val.n))
  accuracy <- append(accuracy, (sum(count)/nrow(val.n)))
  
}
i <- 1
highestVal <- 0
highestInd <- 1
highLen <- 1

for (i in 1:length(accuracy)) {
  if(accuracy[[i]] > highestVal) {
    #print(accuracy[[i]])
    #print(i)
    highestVal <- accuracy[[i]]
    highestInd <- i
    highLen <- i
  }
  if((accuracy[[i]] == accuracy[[highestInd]]) & ((i-highLen) <= 1)) {
    highLen <- i
    #print("highLen:")
    #print(i)
  }
}

highK <- highestInd
#highLen <- lowestInd
#print(highK)
#print(highLen)
k <- as.integer(highK+(highLen-highK)/2)
print("Best k:")
print(k)

print("Best k on test")
count <- apply(test.n, 1, get_uw_acc,k=k,count = 0)
print(sum(count)/nrow(test.n))

sink()
