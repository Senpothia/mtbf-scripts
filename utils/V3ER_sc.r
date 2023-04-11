# Etude statistique lot V3ER - Juillet 2016
# Version avec enregistrement des résultats dans fichier "Sortie"

#sink("Sortie.txt") # Ouverture du fichier de sauvegade des résultats d'analyse

library(Cairo)

plotsFolder<- "/home/miguel/R/mtbf/plots/"
scriptFolder<-"/home/miguel/R/mtbf/utils/"
dataFolder<-"/home/miguel/R/mtbf/data/"
extensions<-c(".csv",".png",".r")


#TAB<-read.table("/home/miguel/R/mtbf/data/Contrecep_01.csv",header=TRUE,sep=";",dec=",")



lecture<-function(fichier){
  
  parth<-getPat(dataFolder, fichier, extension[0])
  TAB<-read.table(fichier,header=TRUE,sep=";",dec=",")
  return(TAB)
  
}

resume<-function(TAB){
  
  colonne <- TAB$daN
  res_daN<-summary(colonne) # Resumé des forces
  return(res_daN)
  
}


# Histogramme
histo<-function (){

tab1<-table(TAB$daN,TAB$Resultat) # Tableau de contingence des forces / résultats
Cairo(file="/home/miguel/R/mtbf/plots/hist.png",
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")
	  
hist(TAB$daN,nclass=8,col="blue",main="Histogramme des forces",xlab="Force - daN",ylab="Effectifs",xlim=c(160,260),ylim=c(0,100),panel.first= grid(NA, 5, lwd = 2))
abline(v=200,col="red",lwd=5)
text(215,90," Seuil de conformite",col='red',cex=1)

dev.off()

}


histogramme<-function (nom){

tab1<-table(TAB$daN,TAB$Resultat) # Tableau de contingence des forces / résultats

nom <-paste(nom,".png")
nom<-paste(plotsFolder,nom)
Cairo(file=nom,
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")
	  

hist(TAB$daN,nclass=8,col="green",main="Histogramme des forces",xlab="Force - daN",ylab="Effectifs",xlim=c(160,260),ylim=c(0,100),panel.first= grid(NA, 5, lwd = 2))
abline(v=200,col="red",lwd=5)
text(215,90," Seuil de conformite",col='red',cex=1)

dev.off()

}



# Etude de normalité

quantiles<-function(){

Cairo(file="/home/miguel/R/mtbf/plots/qqplot.png",
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")


qqnorm(TAB$daN,main="Diagramme Quantiles-Quantile")
qqline(TAB$daN,col="red")
dev.off()

}


densite<-function(){

Cairo(file="/home/miguel/R/mtbf/plots/densite.png",
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")
	  
hist(TAB$daN,nclass=8,proba=TRUE,col="blue",main="Histogramme des forces",xlab="Force - daN",ylab="Denstity",ylim=c(0,0.035))
pts<-seq(170,250,length=75)

lines(pts,dnorm(pts,mean(TAB$daN),sd(TAB$daN)),lwd=2,col="red")
legend(175,0.03,legend=c("Histogramme","N(208,13)"),col=c("blue","red"),lty=c(1,1),cex=0.8)

dev.off()

}

# Etude des valeurs abérrantes

atypiques<-function(){

Cairo(file="/home/miguel/R/mtbf/plots/atypiques.png",
      type="png",
      bg="white",
      units="px", 
      width=650, 
      height=400, 
      pointsize=12, 
      dpi="auto")

box<-boxplot(TAB$daN, main="Boite de dispersion") #Boite de dispersion
dev.off()

}

getPath<-function(folder, file, extension){
	
	file<-paste(file, extension)
	path<-paste(folder, file)
	return path

}

#sink()   # Fermeture du fichier de sauvegade des résultats DC3