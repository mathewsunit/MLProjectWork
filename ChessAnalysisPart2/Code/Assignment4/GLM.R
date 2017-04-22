require(adabag)
require(sigmoid)
#train_com <- subset(train_values, select = -c(funder,gps_height,installer,wpt_name,subvillage,lga,ward,population,scheme_name,construction_year,waterpoint_type_group,waterpoint_type,source_type,source,payment_type,payment,management,extraction_type_class,extraction_type_group,extraction_type,scheme_management,scheme_management,district_code,region_code,region,basin,amount_tsh) )
mydata <-read.csv("/home/sunit/Downloads/kr-vs-kp-data.csv",header = FALSE)
names(mydata)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")
train_values_mod <- mydata
train_values_mod <- as.data.frame(train_values_mod)
#train_values_mod <- sapply(train_values_mod,as.numeric)
#train_values_mod <- as.data.frame(train_values_mod)
#train_values_mod <- train_values_mod[sample(1:nrow(train_values_mod), 50,replace=FALSE),] 
#train_values_mod <- sapply(train_com,as.numeric)
accuracyArray <- {}
areaArray <- {}
folds <- cut(seq(1,nrow(train_values_mod)),breaks=20,labels=FALSE)
for(i in 1:20){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- train_values_mod[index, ]
  trainData <- train_values_mod[-index, ]
  pois.mod <- glm(Class ~ ., data=trainData,family=quasibinomial(link = "logit"))
  #print(pois.mod)
  pois.pred <- predict(pois.mod, newdata = testData[,-37], type="response")
  pois.pred.normal <- pois.pred/10
  pois.pred.normal <- round(pois.pred.normal,digits = 2)
  pois.pred.normal <- ceiling(pois.pred.normal)
  glmPrediction <- roc(pois.pred,testData$Class)
  accuracyArray <- append(accuracyArray,sqrt(mean((floor(pois.pred.normal)-testData[,37])^2)))
  areaArray <- append(areaArray,glmPrediction$auc)
  print(sqrt(mean((floor(pois.pred.normal)-testData[,37])^2)))
}