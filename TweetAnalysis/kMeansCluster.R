#remove some sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method ="centroid")
m3 <- t(m2)
k <- 6
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3)