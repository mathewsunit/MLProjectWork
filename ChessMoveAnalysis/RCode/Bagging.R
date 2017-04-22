require(adabag)
#train_com <- subset(train_values, select = -c(funder,gps_height,installer,wpt_name,subvillage,lga,ward,population,scheme_name,construction_year,waterpoint_type_group,waterpoint_type,source_type,source,payment_type,payment,management,extraction_type_class,extraction_type_group,extraction_type,scheme_management,scheme_management,district_code,region_code,region,basin,amount_tsh) )
mydata <-read.csv("data/kr-vs-kp.data",header = FALSE)
names(mydata)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")
train_values_mod <- mydata
#train_values_mod <- sapply(train_values_mod,as.numeric)
#train_values_mod <- as.data.frame(train_values_mod)
#train_values_mod <- train_values_mod[sample(1:nrow(train_values_mod), 50,replace=FALSE),] 
#train_values_mod <- sapply(train_com,as.numeric)
folds <- cut(seq(1,nrow(train_values_mod)),breaks=20,labels=FALSE)
accuracyArray <- {}
areaArray <- {}
for(i in 1:20){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- train_values_mod[index, ]
  trainData <- train_values_mod[-index, ]
  pred<-bagging(Class~.,data=trainData,boos=TRUE,mfinal=30,rpart.control(cp = -1,minsplit = 10))
  #pred<-boosting(status_group~funder,data=trainData,boos=TRUE,mfinal=5)
  #Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ], newmfinal=10)
  #Vehicle.bagging.pred$confusion
  #Vehicle.bagging.pred$error
  bag.pred <- predict(pred,testData[,-37],probability = TRUE)
  a1 <- rep(0, nrow(testData))
  a2 <- rep(0, nrow(testData))
  a1[which(bag.pred$class == "won")] <- 1
  a1[which(bag.pred$class == "nowin")] <- 0
  a1 <- append(a1,0.5)
  a2[which(testData$Class == "won")] <- 1
  a2[which(testData$Class == "nowin")] <- 0
  a2 <-append(a2,0)
  bag.prediction <- roc(a1,a2)
  #print(plot(bag.prediction))
  print(errorevol(pred,testData))
  accuracyArray <- append(accuracyArray,errorevol(pred,testData)$error)
  areaArray <- append(areaArray,bag.prediction$auc)
}