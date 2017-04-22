# Steve Raftery
# Machine Learning
# Assignment 4

# make sure to set the working directory correctly.

# if you need to install these packages uncomment the 3 lines below.
# install.packages("rpart")
# install.packages("neuralnet")
# install.packages("e1071")

library("rpart")
library("neuralnet")
library("e1071")
library("stats")
require(caTools)

setwd("C:/Users/Steve/Documents/UTD/MachineLearning/assignment4_sxr161130_sxm167631/Assignment4")
df <-read.csv("data/kr-vs-kp-data.csv",header = FALSE)
names(df)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")

#df <- transform(df,Bkblk=as.numeric(Bkblk),Bknwy=as.numeric(Bknwy),Bkon8=as.numeric(Bkon8),Bkona=as.numeric(Bkona),Bkspr=as.numeric(Bkspr),Bkxbq=as.numeric(Bkxbq),Bkxcr=as.numeric(Bkxcr),Bkxwp=as.numeric(Bkxwp),Blxwp=as.numeric(Blxwp),Bxqsq=as.numeric(Bxqsq),Cntxt=as.numeric(Cntxt),Dsopp=as.numeric(Dsopp),Dwipd=as.numeric(Dwipd),Hdchk=as.numeric(Hdchk),Katri5=as.numeric(Katri5),Mulch=as.numeric(Mulch),Qxmsq=as.numeric(Qxmsq),R2ar8=as.numeric(R2ar8),Reskd=as.numeric(Reskd),Reskr=as.numeric(Reskr),Rimmx=as.numeric(Rimmx),Rkxwp=as.numeric(Rkxwp),Rxmsq=as.numeric(Rxmsq),Simpl=as.numeric(Simpl),Skach=as.numeric(Skach),Skewr=as.numeric(Skewr),Skrxp=as.numeric(Skrxp),Spcop=as.numeric(Spcop),Stlmt=as.numeric(Stlmt),Thrsk=as.numeric(Thrsk),Wkcti=as.numeric(Wkcti),Wkna8=as.numeric(Wkna8),Wknck=as.numeric(Wknck),Wkovl=as.numeric(Wkovl),Wkpos=as.numeric(Wkpos),Wtoeg=as.numeric(Wtoeg),Class=as.numeric(Class))
#mydataPreProcessed <- preProcess(mydata[,-1],method = c("center","scale"))

rowCount = nrow(df);
rowsInSample = floor(rowCount / 10 );
for( i in 1:10){
  beginIndex = (i-1) * rowsInSample + 1;
  endIndex = i * rowsInSample;
  test = df[beginIndex:endIndex,];
  tmpTrain1 = df[1:beginIndex,]
  tmpTrain2 = df[endIndex:rowCount,]
  train <- rbind(tmpTrain1,tmpTrain2)
  testFileName = paste("test",i,".dat")
  trainFileName = paste("train",i,".dat")
  write.table(test, file = testFileName,append=FALSE,sep = "\t")
  write.table(train, file = trainFileName,append=FALSE,sep = "\t")
  if(i <= 5){
    lrmodel = glm(Class ~ ., data=train,family=poisson(link="log"))
  }else{
    lrmodel = glm(Class ~ ., data=train,family=binomial(link = "logit"))
  }
  pred<- predict(lrmodel,newdata = test[,-37], type="response");
  lrFile = paste("lr",i,".dat")
  write.table(pred, file = lrFile,append=FALSE,sep = "\t")
}
