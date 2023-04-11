# Etude statistique lot V3ER - Juillet 2016



TAB<-read.table("/home/miguel/R/mtbf/data/Contrecep_01.csv",header=TRUE,sep=";",dec=",")

res_daN<-summary(TAB$daN) # Resumé des forces
cat("********************************","\n","Bilan des mesures de forces d'arrachement:","\n")
print(res_daN)
cat("\n","Ecart-type:",sd(TAB$daN),"\n")

# Tableau de contingence
# cat("********************************","\n","\n","Tableau de contingence:","\n")
tab1<-table(TAB$daN,TAB$Resultat) # Tableau de contingence des forces / résultats
# print(tab1) # Affichage du tableau de contingence

# Histogramme

hist(TAB$daN,nclass=8,col="blue",main="Histogramme des forces",xlab="Force - daN",ylab="Effectifs",xlim=c(160,260),ylim=c(0,80),panel.first= grid(NA, 5, lwd = 2))

r
abline(v=200,col="red",lwd=5)
text(185,76," Seuil de conformite",col='red',cex=1)

# Etude de normalité
X11()
qqnorm(TAB$daN,main="Diagramme Quantile-Quantile")
qqline(TAB$daN,col="red")

# Densité déduite
X11()
hist(TAB$daN,nclass=8,proba=TRUE,col="blue",main="Histogramme des forces",xlab="Force - daN",ylab="Denstity",ylim=c(0,0.035))
pts<-seq(170,250,length=75)
# densite<-dnorm(pts,mean(TAB$daN),sd(TAB$daN))
lines(pts,dnorm(pts,mean(TAB$daN),sd(TAB$daN)),lwd=2,col="red")
legend(175,0.03,legend=c("Histogramme","N(207.9,13.34)"),col=c("blue","red"),lty=c(1,1),cex=0.8)
# Vérification de conformité

Conforme<-TAB$daN[TAB$daN>=200]
# print(paste("Conf:",Conforme)) # Affichage des résultats conformes

cat("\n","\n","*******************************","\n","Bilan des resultats:","\n","\n")
print("Critere de conformite: force d'arrachement superieure ou egale a 200 daN")
print(paste("Nombre de pieces testees:",length(TAB$daN)))
print(paste("Nombre de ventouses conformes:", length(Conforme),"soit:",round(length(Conforme)/length(TAB$daN),2)*100,"%"))

cat("\n","Repartions des forces:","\n")
classe<-cut(TAB$daN,breaks=seq(170,250,by=10))
print(table(classe))

# Etude des valeurs abérrantes
X11()
box<-boxplot(TAB$daN, main="Boite de dispersion") #Boite de dispersion

atypique<-box$out # Identification des valeurs atypiques

cat("\n","*****************************","\n","Valeurs atypiques:",atypique)
num_atypique<-which((TAB$daN)%in%atypique) # Recherche l'ordre des valeurs atypiques détectées

cat("\n","*****************************","\n","Numero des valeurs atypiques:",num_atypique,"\n")

res_daN_2<-box$stats

# Affichage des résultats depuis la boite de dispersion

cat("\n","****************************","\n","Resume des resultats","\n")
print(paste("1er quartile:",res_daN_2[2,]))
print(paste("Mediane:",res_daN_2[3,]))
print(paste("3eme quartile:",res_daN_2[4,]))
print(paste("Max:",res_daN_2[5,]))

