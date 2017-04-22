require(neuralnet)
require(ROCR)
require(rpart)
require(rpart1a)
require(e1071)
require(caret)
require(partykit)
require(rattle)
mydata <-read.csv("data/kr-vs-kp.data",header = FALSE)
names(mydata)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")
mydata <- transform(mydata,Bkblk=as.numeric(Bkblk),Bknwy=as.numeric(Bknwy),Bkon8=as.numeric(Bkon8),Bkona=as.numeric(Bkona),Bkspr=as.numeric(Bkspr),Bkxbq=as.numeric(Bkxbq),Bkxcr=as.numeric(Bkxcr),Bkxwp=as.numeric(Bkxwp),Blxwp=as.numeric(Blxwp),Bxqsq=as.numeric(Bxqsq),Cntxt=as.numeric(Cntxt),Dsopp=as.numeric(Dsopp),Dwipd=as.numeric(Dwipd),Hdchk=as.numeric(Hdchk),Katri5=as.numeric(Katri5),Mulch=as.numeric(Mulch),Qxmsq=as.numeric(Qxmsq),R2ar8=as.numeric(R2ar8),Reskd=as.numeric(Reskd),Reskr=as.numeric(Reskr),Rimmx=as.numeric(Rimmx),Rkxwp=as.numeric(Rkxwp),Rxmsq=as.numeric(Rxmsq),Simpl=as.numeric(Simpl),Skach=as.numeric(Skach),Skewr=as.numeric(Skewr),Skrxp=as.numeric(Skrxp),Spcop=as.numeric(Spcop),Stlmt=as.numeric(Stlmt),Thrsk=as.numeric(Thrsk),Wkcti=as.numeric(Wkcti),Wkna8=as.numeric(Wkna8),Wknck=as.numeric(Wknck),Wkovl=as.numeric(Wkovl),Wkpos=as.numeric(Wkpos),Wtoeg=as.numeric(Wtoeg),Class=as.numeric(Class))
mydataPreProcessed <- preProcess(mydata[,-1],method = c("center"))
mydata <- predict(mydataPreProcessed,mydata[,-1])
#mydata<-mydata[sample(nrow(mydata)),]
folds <- cut(seq(1,nrow(mydata)),breaks=20,labels=FALSE)
vectornn1 = c()
for(i in 1:20){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- mydata[index, ]
  trainData <- mydata[-index, ]
  n <- names(trainData)
  f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))
  net.sqrt <- neuralnet(f,data = trainData, hidden=5, threshold=.01)
  nnPred <- compute(net.sqrt,testData[,-36])
  print(sqrt(mean(nnPred$net.result-testData$Class)^2))
  vectornn1 <- c(vectornn1, sqrt(mean(nnPred$net.result-testData$Class)^2))
  #rpartPred <- prediction(net.sqrt,testData)
  #print(confusionMatrix(rpartPred,testData$Class))
  #fit.pr = predict(svp,testData,type="prob")[,2]
  #fit.pred = prediction(fit.pr,testData$Class)
  #fit.perf = performance(fit.pred,"tpr","fpr")
  #plot(fit.perf,lwd=2,col="blue",
  #     main="ROC:  Classification Trees on Adult Dataset")
  #abline(a=0,b=1)
}