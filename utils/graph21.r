
# save histogram in pdf format in current directory
 
library(Cairo) 
graph<-function(){
Cairo(file="/home/miguel/R/mtbf/plots/testfile21.png",
      type="png",
      units="px", 
      width=400, 
      height=300, 
      pointsize=12, 
      dpi="auto")

# a histogram we want to save
hist(airquality$Temp)

# call this function to save the file 
dev.off()
}


