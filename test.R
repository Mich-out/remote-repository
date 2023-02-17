Produc <- read.csv("C:/Users/HP OMEN/Desktop/2A/SAT/SAT-MEGA - R/GxE_ProdMais.csv", sep=";", dec=".",stringsAsFactors=TRUE)

library(ggplot2)
library(car)
ggplot(Produc, aes(Gen,Env))+geom_boxplot(aes(col=Gen))+labs(title="Modèle") 

ggplot(Produc, aes(Gen, Prod, fill=Gen, fixed=FALSE))+
  geom_boxplot()+
  facet_wrap(~Env)+
  labs(title="Rendements de 10 génotypes de maïs en fonction de 5 environnements différents")+
  xlab("Génotypes")+
  ylab("Production (q/ha)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size=9))+
  theme(legend.title = element_blank(), legend.justification = c("right","bottom"))

model=lm(Prod~Env*Gen,data = Produc)
leveneTest(model)
#Indépendance des résidus (quand valeurs globalements centrés sur 0)
plot(model,1)
#Normalité des résidus (quand points le long de la ligne, résidus suivent une loi normale)
plot(model,2)
#TEST de shapiro HO:normal (<0.05 H0 pas vérifié suivent pas une loi normale) 
shapiro.test(resid(model))
#Homogénéité des variences des risidus (courbe droite homogénéité des variances des résidus)
plot(model,3)

#Faire un graphique interaction Genotype,Environnement en fonction de la production 
interaction.plot(Produc$Env,Produc$Gen,Produc$Prod,col=1:11,trace.label="Génotypes",xlab="Environnement",ylab="Production",lty=1,fixed=TRUE, main="Production de différentes variétés de mais en fonction de l'environnement")


########################################################################################

library(car) 
library(datasets)
library(dplyr)

#Utilisation de BOXCOx trouver la valuer de la puissance à laquelle elevé les données pour améliore, transformer le modèle en boxcos pour que ca fonctionne car hypothèse si pas modif pas vérifiées 
summary(p1 <-powerTransform(model))
Produc_bc <- transform(Produc, Prod_bc=bcPower(Prod,coef(p1)))
head(Produc_bc)

mod_bc <- lm(Prod_bc~  Gen*Env, 
             contrasts=list(Gen=contr.sum, Env=contr.sum),
             data=Produc_bc) 
summary(mod_bc)

plot(mod_bc, 1)
plot(mod_bc, 2)
plot(mod_bc, 3)
plot(mod_bc, 4)
par(mfrow=c(1,1))

#test de normalité (si p_value > 0.05 normalité des résidus)
shapiro.test(residuals(mod_bc))

#Tester l'homogénéité des variances des résidus (homogénéité si p_value>0.05)
bartlett.test(residuals(mod_bc)~Produc_bc$condition)
Produc_bc$condition <- interaction(Produc_bc$Gen, Produc_bc$Env, sep="_")
leveneTest(residuals(mod_bc)~Produc_bc$condition)
leveneTest(mod_bc)
#Affichage de la table Anova 
aov <- Anova(mod_bc,type=3)
summary(aov)
aov
#Interaction signification Test de Tukey (si p_value > 0.05 différence significative entre les moyennes)
model2=aov(Prod~Env*Gen,data = Produc_bc)
model2
#TukeyHSD(model2, conf.level = 0.99)
#plot(TukeyHSD(model2, conf.level = 0.99),las=1, col = "red")
library(agricolae)

out <- HSD.test(model2,c("Env","Gen"), group=TRUE,console=TRUE)
plot(out, las=2, main ="Comparaison des moyennes des différentes interactions", ylab = "Production (q/ha)")
