library(readr)
library(tidyverse)
library(ggcorrplot)
library(mclust)

dataset<- read_csv("Life-Expectancy-Data-Updated.csv")
View(dataset)
str(dataset)

anyNA(dataset)

data_num<-dataset[-c(1,2,3)]  #le due variabili dummy le devo tenere?
correlazioni <- round(cor(data_num),3)
ggcorrplot(correlazioni, hc.order = T, lab=T)#togliamo polio, adult mortality, infant deaths, under five deaths


dataset_correlati<-dataset[,c(4,5,6,11,12,16,17,21)]
correlazioni2<-round(cor(dataset_correlati),3)
ggcorrplot(correlazioni2, hc.order = T, lab=T)

data_num<-data_num[,-c(1,2,3,8,12,13,16,17)]
dataset<-dataset[,-c(4,5,6,11,15,16,19,20)] #togliamo le variabili che sono correlate tra loro e populationsiccome secondo la nostra analisi 
#non è una variabile che discrimina l'aspettativa di vita come poteva essere la densità.

correlazioni <- round(cor(data_num),3)
ggcorrplot(correlazioni, hc.order = T, lab=T)


classi_life<-dataset$Life_expectancy
classi_life<-cut(classi_life, breaks = c(-Inf,60,70,78, Inf), labels = c("<60","60-70","70-78",">78"))
dataset<-cbind(dataset, classi_life)
data_num_labels<-cbind(data_num, classi_life)


#standardiziamo GDP
dataset$GDP_per_capita <- ((dataset$GDP_per_capita - min(dataset$GDP_per_capita)) / (max(dataset$GDP_per_capita) - min(dataset$GDP_per_capita)))
data_num$GDP_per_capita <- ((data_num$GDP_per_capita - min(data_num$GDP_per_capita)) / (max(data_num$GDP_per_capita) - min(data_num$GDP_per_capita)))

ggplot(data = dataset, mapping = aes(x = reorder(Region, Life_expectancy, FUN = mean), y = Life_expectancy, color=Region)) +
  geom_boxplot() +
  coord_flip()+
  theme(legend.position = "none")+
  labs(y = "Life Expectancy", x = "")


data_num %>% 
  gather(key = key, value = value) %>% 
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_histogram(colour = "black") +
  facet_wrap(~key, scales = "free", ncol = 5) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Values", y = "Count")#La maggior parte delle variabili ha una distribuzione distorta,BMI sembra avere una distribuzione bimodale.




data_num%>% 
  gather(key = key, value = value) %>% 
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(y = value, fill=key)) +
  geom_boxplot(colour = "black") +
  facet_wrap(~key, scales = "free",ncol=5) +
  theme_classic()+
  theme(legend.position = "none")  #qausi tutte le variabili hanno outliers.

ggplot(dataset, aes(x = classi_life, y = Life_expectancy, fill = classi_life)) + 
  geom_violin(alpha = 0.7) +
  geom_boxplot(alpha = 0.1, outlier.colour = "black")+
  theme(legend.position = "none")+
  labs(x = "Classi", y = "Life Expectancy")

data_num_labels%>% 
  gather(key = key, value = value, -classi_life) %>% 
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(x=classi_life,y = value, fill=key)) +
  geom_boxplot(colour = "black") +
  facet_wrap(~key, scales = "free",ncol=5) +
  theme_classic()+
  theme(legend.position = "none") 






###########################################################################
#--------------------------------------------
# Model-based clustering fitting and plotting
#--------------------------------------------
###########################################################################

globo.Mclust2<-Mclust(data_num[,-c(10)])
summary(globo.Mclust2)

globo.Mclust2$BIC
#Top 3 models based on the BIC criterion: 
#  VVV,9      VVV,8      VVV,7 
#-98925.48  -99859.15 -101261.02 

globo.mclustICL <- mclustICL(data_num[,-c(10)])
globo.mclustICL
#Top 3 models based on the ICL criterion: 
#  VEV,9     VEV,8     VVE,8 
#-100022.4 -100520.0 -101895.1 

par(mfrow = c(1, 1))
plot(globo.Mclust2, what = "BIC")#, legendArgs = list(x = "topleft"))# legendArgs=list("bottom", text='My values')
plot (globo.mclustICL)

clust_9<-factor(globo.Mclust2$classification,levels = c(1,2,3,4,5,6,7,8,9), labels = c("Middle East" , "European Union" ,"Asia"                         
                                                                                                  ,"South America","Central America and Caribbean", "Rest of Europe"               
                                                                                                  , "Africa","Oceania","North America" ))
classError(clust_9, class=dataset$Region)#error rate altissimo
#i nove gruppi che ci segnalava Mclust non corrispondono alle regioni.

#proviamo a vedere cosa succede all'aumentare del numero di gruppi


globo.Mclust3<-Mclust(data_num[,-c(10)],G=5:15)
globo.Mclust3$BIC  
#Top 3 models based on the BIC criterion: 
#  VEV,15    VEV,14    VEV,13 
#-96345.98 -97683.44 -98140.88 

#notiamo che con l'aumentare del numero massimo di gruppi anche il numero di gruppi per il
#modello migliore aumenta, segno che l'algoritmo non trova dei gruppi sensati all'interno del dataset 

#provo a mettere 4 gruppi come i tre gruppi della variabile life expectancy

life.Mclust<-Mclust(data_num[,-c(10)],G=1:4)
summary(life.Mclust)

 
life.Mclust$BIC
#Top 3 models based on the BIC criterion: 
#  VVV,4     EVV,4     VEV,4 
#-107173.2 -108394.6 -109846.0 

plot(life.Mclust$BIC)


life.MclustICL<-mclustICL(data_num[,-c(10)],G=1:4)
life.MclustICL
#Top 3 models based on the ICL criterion: 
#  VVV,4     EVV,4     VEV,4 
#-107312.0 -108760.3 -110038.6
plot(life.MclustICL)

  
cluster_life<-life.Mclust$classification


classError(cluster_life, class=classi_life)#error rate 0.35


par(mfrow = c(1, 2))
dataset_df<-as.data.frame(dataset)
coordProj (dataset_df, dimens=c(4,12), what="classification",
           classification=classi_life,
           col=c("red2","dodgerblue2","green3","yellow"), symbols=c(17,0,16,2),
           sub="true classification")
legend("bottomright", legend = c("<60","60-70","70-78",">78"),
       col = c("red2", "dodgerblue2", "green3", "yellow"), pch = c(17, 0, 16, 2))

coordProj (dataset_df, dimens=c(4,12), what="classification",
           classification=cluster_life,
           col=c("red2","dodgerblue2","green3","yellow"), symbols=c(17,0,16,2),
           sub="Model-Based Clustering")

# + missclassified
#-----------------
(miss_class<-classError(cluster_life, class=classi_life)$misclassified)
points(dataset[miss_class,c(4,12)],pch=19)


par(mfrow = c(1, 1))

# Uncertainty plot
#------------------
uncerPlot (z=life.Mclust$z,truth=classi_life)
# Plot mclust uncertainty
coordProj (data=dataset_df, dimens=c(4,12), what="uncertainty",
           parameters=life.Mclust$parameters , z=life.Mclust$z)


# MDA
#------------------
test<-dataset %>%
  filter(Year=="2015")    #prendo la obs di ogni nazione nell'anno 2015
test
testg<-test
train<-dataset %>%    #elimino il test set dal dataset per ottenere il train
  anti_join(test)
train


 
set.seed(213)
x.train <- train[-c(1,2,3,13,14)]
y.train <- unlist (train[14])


mod = MclustDA(x.train, y.train)
summary(mod)  
str(mod)   


fit<-predict(mod,test[-c(1,2,3,13,14)])$class
n<-nrow(test)
sum(as.tibble(fit) !=test[14])/n

testg[14]<-as.tibble(fit)


##GLOBO


#codice grafici globo
world_tbl<-map_data("world")%>%  
  as_tibble()%>%
  filter(region != "Antarctica")
world_tbl$region<-recode(world_tbl$region,"USA" = "United States")
world_tbl$region<-recode(world_tbl$region,"Turkey" = "Turkiye")
world_tbl$region<-recode(world_tbl$region,"Syria" = "Syrian Arab Republic")
world_tbl$region<-recode(world_tbl$region,"Iran" = "Iran, Islamic Rep.")
world_tbl$region<-recode(world_tbl$region,"Laos" = "Lao PDR")
world_tbl$region<-recode(world_tbl$region,"Kyrgyzstan" = "Kyrgyz Republic")
world_tbl$region<-recode(world_tbl$region,"Czech Republic" = "Czechia")
world_tbl$region<-recode(world_tbl$region,"Slovakia" = "Slovak Republic")

world_tbl$region<-recode(world_tbl$region,"Ivory Coast" = "Cote d'Ivoire")
world_tbl$region<-recode(world_tbl$region,"Venezuela" = "Venezuela, RB")
world_tbl$region<-recode(world_tbl$region,"Russia" = "Russian Federation")
world_tbl$region<-recode(world_tbl$region,"Egypt" = "Egypt, Arab Rep.")
world_tbl$region<-recode(world_tbl$region,"UK" = "United Kingdom")
world_tbl$region<-recode(world_tbl$region,"Yemen" = "Yemen, Rep.")
world_tbl$region<-recode(world_tbl$region,"Republic of Congo" = "Congo, Rep.")
world_tbl$region<-recode(world_tbl$region,"Democratic Republic of the Congo" = "Congo, Dem. Rep.")
world_life_tblr<-test  %>%
  right_join(world_tbl,by=c("Country"="region"))

world_life_tblr%>%                               
  ggplot(aes(long,lat))+   
  geom_polygon(aes(group=group,fill=classi_life))+
  scale_fill_brewer(palette=5,na.value = "lightgrey")


#valori stimati
world_life_tbl<-testg %>%
  right_join(world_tbl,by=c("Country"="region"))

world_life_tbl%>%                               
  ggplot(aes(long,lat))+   
  geom_polygon(aes(group=group,fill=classi_life))+
  scale_fill_brewer(palette=5,na.value = "lightgrey")

# EDDA
#------------------

mod2 <- mixmodLearn(x.train, y.train, 
                    models=mixmodGaussianModel(family="all",equal.proportions=FALSE), #stimiamo tutti modelli
                    criterion=c('CV','BIC'))
mod2

#Miglior modello EVE
mod2@results[[1]]@model   #Gaussian_pk_L_D_Ak_D
mod2@results[[1]]@criterionValue[1]
mod2@results[[1]]@criterionValue[2]
#Secondo EEE
mod2@results[[2]]@model   #Gaussian_pk_L_C 
#Terzo VVV
mod2@results[[3]]@model   #Gaussian_pk_Lk_Ck 

# Colezziono valori di BIC e CV 
BIC = CV = rep(NA ,length(mod2@models@listModels) )

for (i in 1: length(mod2@models@listModels)){
  ind = which(mod2@results [[i]] @model == mod2@models@listModels) #nel elenco di 14 mod al passo i
  CV[ind] = mod2@results [[i]] @criterionValue [1]
  BIC[ind] = mod2@results [[i]] @criterionValue [2]
}
round(BIC,1)  #tutti valori
min(BIC)      # 113284.7  
which.min(BIC)# 14

round(CV,3) 
min(CV)       # 0.1832402
which.min(CV) # 9

# Grafico CV e BIC di tutti modelli
#-------------------------------
par(mfrow=c(2,1))
plot(BIC ,type='b',xlab='',xaxt='n',col =2); axis(1,at=1: length(
  mod2@results),labels=substr(mod2@models@listModels ,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(BIC), col=1, lty =2)

plot(CV ,type='b',xlab='',xaxt='n',col =3); axis(1,at=1: length(
  mod2@results),labels=substr(mod2@models@listModels,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(CV), col=1, lty =2)
par(mfrow=c(1,1))

#-----------graficoc alternativo
#ggplot(data = data.frame(CV = CV, BIC = BIC), aes(x = 1:length(CV))) + 
#  geom_line(aes(y = CV, colour = CV)) + 
#  geom_line(aes(y = BIC, colour = BIC)) + 
#  geom_vline(xintercept = which.min(CV), linetype = 2) + 
#  geom_vline(xintercept = which.min(BIC), linetype = 2) + 

#Predizione EDDA
pred<-mixmodPredict(data=test[-c(1,2,3,13,14)], classificationRule=mod2["bestResult"])

pred2<-mixmodPredict(data=test[-c(1,2,3,13,14)], classificationRule=mod2@results[[2]])

pred3<-mixmodPredict(data=test[-c(1,2,3,13,14)], classificationRule=mod2@results[[3]])

# valuto la preformance delle predizione 
test[14] 
pred["partition"]

#erorr rate
mean(as.integer(test[14] == pred["partition"]))
mean(as.integer(test[14] == pred2["partition"]))
mean(as.integer(test[14] == pred3["partition"]))
g<-test$classi_life
gg<-as.factor(pred3["partition"])
confusionMatrix(data=g,reference=gg)

