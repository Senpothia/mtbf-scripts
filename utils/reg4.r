library(Cairo) 
library(ggplot2)
graph<-function(){
  
  
  Cairo(file="/home/miguel/R/mtbf/plots/regression4.png",
        type="png",
        bg="white",
        units="px", 
        width=650, 
        height=400, 
        pointsize=12, 
        dpi="auto")
  
  
  Obs<-1:15
  Age<-c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
  TxDDT<-c(0.2,0.25,0.18,0.19,0.29,0.28,0.31,0.33,0.36,0.71,0.38,0.47,1.1,0.87,0.83)
  broches<-data.frame(Obs, Age, TxDDT)
  model<-lm(TxDDT~Age)
  print(model)
  plot<-ggplot(broches, aes(x = Age, y = TxDDT)) +
    geom_point() +
    stat_smooth(method = "lm")
  print(plot)
  
  # call this function to save the file 
  dev.off()
}
