
# save histogram in pdf format in current directory
 
library(Cairo) 
Cairo(file="testfile2.jpg",
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



