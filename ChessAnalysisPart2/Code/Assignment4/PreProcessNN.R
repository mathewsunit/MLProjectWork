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
#dev.new()
#pdf(file="test.pdf")
#i <- 1
#for (column in mydata) {
#  hist(column,main=paste(colnames(mydata)[i]))
#  i <- i +1
#}
#dev.off()