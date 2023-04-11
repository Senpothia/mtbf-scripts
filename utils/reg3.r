library(Cairo) 

graph<-function(){

TAB<-read.table("/home/miguel/R/mtbf/data/reg3.csv",header=TRUE,sep=";",dec=",")

Cairo(file="/home/miguel/R/mtbf/plots/regression31.png",
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")

broches<-data.frame(TAB$Obs, TAB$Age, TAB$TxDDT)
model<-lm(TAB$TxDDT~TAB$Age)
#print(model)
plot(TAB$Age,TAB$TxDDT)
abline(model)
segments(TAB$Age,fitted(model),TAB$Age, TAB$TxDDT)
pred.frame<-data.frame(TAB$Age)
pc<-predict(model, interval="confidence",
            newdata=pred.frame)
pp<-predict(model, interval="prediction",
            newdata=pred.frame)
matlines(pred.frame, pc[,2:3], lty=c(2,2), col="blue")
matlines(pred.frame, pp[,2:3], lty=c(3,3), col="red")
legend("topleft",c("confiance","prediction"),lty=c(2,3)
       , col=c("blue","red"))
# call this function to save the file 
dev.off()
}
