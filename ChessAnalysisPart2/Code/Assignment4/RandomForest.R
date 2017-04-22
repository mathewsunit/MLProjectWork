require(adabag)
require(randomForest)
#train_com <- subset(train_values, select = -c(funder,gps_height,installer,wpt_name,subvillage,lga,ward,population,scheme_name,construction_year,waterpoint_type_group,waterpoint_type,source_type,source,payment_type,payment,management,extraction_type_class,extraction_type_group,extraction_type,scheme_management,scheme_management,district_code,region_code,region,basin,amount_tsh) )
mydata <-read.csv("data/kr-vs-kp.data",header = FALSE)
names(mydata)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")
train_values_mod <- mydata
accuracyArray <- {}
areaArray <- {}
#train_values_mod <- sapply(train_values_mod,as.numeric)
#train_values_mod <- as.data.frame(train_values_mod)
#train_values_mod <- train_values_mod[sample(1:nrow(train_values_mod), 50,replace=FALSE),] 
#train_values_mod <- sapply(train_com,as.numeric)
folds <- cut(seq(1,nrow(train_values_mod)),breaks=20,labels=FALSE)
for(i in 1:20){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- train_values_mod[index, ]
  trainData <- train_values_mod[-index, ]
  rf<-randomForest(Class ~ ., data = trainData,ntree=30,mtry=100,maxnodes=50)
  rf.pred <- predict(rf, newdata = testData)
  randTable <- table(rf.pred, testData$Class)
  print(randTable)
  #pred<-boosting(status_group~funder,data=trainData,boos=TRUE,mfinal=5)
  #Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ], newmfinal=10)
  #Vehicle.bagging.pred$confusion
  #Vehicle.bagging.pred$error
  print(sum(diag(randTable))/nrow(testData))
  accuracyArray <- append(accuracyArray,sum(diag(randTable))/nrow(testData))
  a1 <- rep(0, nrow(testData))
  a2 <- rep(0, nrow(testData))
  a1[which(rf.pred == "won")] <- 1
  a1[which(rf.pred == "nowin")] <- 0
  a1 <- append(a1,0.5)
  a2[which(testData$Class == "won")] <- 1
  a2[which(testData$Class == "nowin")] <- 0
  a2 <- append(a2,0)
  rf.prediction <- roc(a1,a2)
  plot(rf.prediction)
  areaArray <- append(areaArray,rf.prediction$auc)
}