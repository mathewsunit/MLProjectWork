for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  test<-as.data.frame(kmeansResult$cluster==i)
  arrayIndex <- which(test$`kmeansResult$cluster == i`==TRUE)
  cat(paste("cluster ", i, ": ", sep = ""))
  cat(dat[arrayIndex,11])
  cat("\n")
}