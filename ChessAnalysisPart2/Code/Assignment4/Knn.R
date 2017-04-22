require(class)
require(pROC)
#train_com <- merge(train_values,train_lables)
#train_com <- subset(train_values, select = -c(funder,gps_height,installer,wpt_name,subvillage,lga,ward,population,scheme_name,construction_year,waterpoint_type_group,waterpoint_type,source_type,source,payment_type,payment,management,extraction_type_class,extraction_type_group,extraction_type,scheme_management,scheme_management,district_code,region_code,region,basin,amount_tsh) )
train_com <- mydata
train_values_mod <- sapply(train_com,as.numeric)
train_values_mod <- as.data.frame(train_values_mod)
folds <- cut(seq(1,nrow(train_values_mod)),breaks=20,labels=FALSE)
#for(name in names(train_com))
#{
accuracyArray <- {}
areaArray <- {}
for(i in 1:20){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- train_values_mod[index, ]
  trainData <- train_values_mod[-index, ]
  knn.pred<-knn(trainData, testData, trainData[,37], k = 28, prob=TRUE)
  a1 <- rep(0, nrow(testData))
  a2 <- rep(0, nrow(testData))
  a1[which(knn.pred == "won")] <- 1
  a1[which(knn.pred == "nowin")] <- 0
  a1 <- append(a1,0.5)
  a2[which(testData$Class == "won")] <- 1
  a2[which(testData$Class == "nowin")] <- 0
  a2 <- append(a2,0)
  knn.prediction <- roc(a1,a2)
  print(mean(knn.pred==testData[, 37]))
  accuracyArray <- append(accuracyArray,mean(knn.pred==testData[, 37]))
  areaArray <- append(areaArray,knn.prediction$auc)
}