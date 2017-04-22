require(stats)
install.packages("caret",dependencies=TRUE)
install.packages("rpart1a")
# install.packages("neuralnet")
install.packages("e1071")
installed.packages();
if(!"ROCR" %in% rownames(installed.packages()))
{install.packages("ROCR",dependencies=TRUE)}
if(!"rpart1a" %in% rownames(installed.packages()))
{install.packages("rpart1a",dependencies=TRUE)}
if(!"caret" %in% rownames(installed.packages()))
{install.packages("caret",dependencies=TRUE)}
if(!"rattle" %in% rownames(installed.packages()))
{install.packages("rattle",dependencies=TRUE)}
if(!"partykit" %in% rownames(installed.packages()))
{install.packages("partykit",dependencies=TRUE)}

library(caret)

library(caret)
require(class)

require(neuralnet)
require(ROCR)
require(rpart)
require(rpart1a)
require(e1071)
require(caret)
require(partykit)
require(rattle)
setwd("C:\\Users\\Steve\\Documents\\UTD\\MachineLearning\\assignment4_sxr161130_sxm167631\\Assignment4")
mydata <-read.csv("data/kr-vs-kp.data",header = FALSE)
names(mydata)<-c("Bkblk","Bknwy","Bkon8","Bkona","Bkspr","Bkxbq","Bkxcr","Bkxwp","Blxwp","Bxqsq","Cntxt","Dsopp","Dwipd","Hdchk","Katri5","Mulch","Qxmsq","R2ar8","Reskd","Reskr","Rimmx","Rkxwp","Rxmsq","Simpl","Skach","Skewr","Skrxp","Spcop","Stlmt","Thrsk","Wkcti","Wkna8","Wknck","Wkovl","Wkpos","Wtoeg","Class")
mydata <- transform(mydata,Bkblk=as.numeric(Bkblk),Bknwy=as.numeric(Bknwy),Bkon8=as.numeric(Bkon8),Bkona=as.numeric(Bkona),Bkspr=as.numeric(Bkspr),Bkxbq=as.numeric(Bkxbq),Bkxcr=as.numeric(Bkxcr),Bkxwp=as.numeric(Bkxwp),Blxwp=as.numeric(Blxwp),Bxqsq=as.numeric(Bxqsq),Cntxt=as.numeric(Cntxt),Dsopp=as.numeric(Dsopp),Dwipd=as.numeric(Dwipd),Hdchk=as.numeric(Hdchk),Katri5=as.numeric(Katri5),Mulch=as.numeric(Mulch),Qxmsq=as.numeric(Qxmsq),R2ar8=as.numeric(R2ar8),Reskd=as.numeric(Reskd),Reskr=as.numeric(Reskr),Rimmx=as.numeric(Rimmx),Rkxwp=as.numeric(Rkxwp),Rxmsq=as.numeric(Rxmsq),Simpl=as.numeric(Simpl),Skach=as.numeric(Skach),Skewr=as.numeric(Skewr),Skrxp=as.numeric(Skrxp),Spcop=as.numeric(Spcop),Stlmt=as.numeric(Stlmt),Thrsk=as.numeric(Thrsk),Wkcti=as.numeric(Wkcti),Wkna8=as.numeric(Wkna8),Wknck=as.numeric(Wknck),Wkovl=as.numeric(Wkovl),Wkpos=as.numeric(Wkpos),Wtoeg=as.numeric(Wtoeg),Class=as.numeric(Class))
mydataPreProcessed <- preProcess(mydata[,-1],method = c("center","scale"))

model = glm(Bkblk ~ Class, data = mydata)
summary(model);


mydata <- predict(mydataPreProcessed,mydata[,-1])
#mydata<-mydata[sample(nrow(mydata)),]
folds <- cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)
vector = c()
for(i in 1:5){
  #Folds
  index <- which(folds==i,arr.ind=TRUE)
  testData <- mydata[index, ]
  trainData <- mydata[-index, ]
  n <- names(trainData)
  nb_model <- naiveBayes(as.factor(Class)~.,data = trainData,eps = 1,threshold = 1, laplace = 1)
  nnPred <- predict(nb_model,testData[,-1])
  pred <- table(nnPred,testData[,36])
  print(sum(diag(pred))/sum(pred))
  vector <- c(vector,sum(diag(pred))/sum(pred))
  #print(sqrt(mean(nnPred==testData$Class)))
  #vector <- c(vector, sqrt(mean(nnPred==testData$Class)))
}


