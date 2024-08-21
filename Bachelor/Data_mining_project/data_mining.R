library(gmodels)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(MASS)
library(gridExtra)
library(tidyverse)
library(car)
library(class)
library(caret)
library(ROCR)
library(heplots)
library(magrittr)
library(dplyr)
library(tidyr)
library(crosstable)
library(fastDummies)
dati<-read.csv("bank-additional-full.csv",header = T, sep = ";")

#MATERIALI E METODI

num_na=apply(dati, 2, function(x) sum(is.na(x)))
num_na
#nessun dato mancante

str(dati)
summary(dati[, c(1,11:14,16:20)])

#per iniziare la nostra analisi mappiamo le distribuzioni delle variabili non numeriche.
library(inspectdf)
dati %>%
  inspect_cat() %>%
  show_plot()

#-------2.1 CONSIDERAZIONI SULLE VARIABILI -----------------------
#(ANALISI PRELIMINARE DEI DATI)
dati<-dati[,-c(9:12)]
#togliamo le variabili che non ci serviranno durante la nostra esami
#Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known
#infatti noi decidiamo di chiamare quelli che secondo la nostra previsione diranno di sì
sum(dati[,13]!=999 & dati[,17]=="yes")#l Variabile risposta è sbilanciata, solo 4649 yes.
#esploriamo singolarmente le variabili

#age
hist(dati$age)
abline(v=c(30,60),col="red", lty=2, lwd=3)

dati = dati %>%
  mutate(age = ifelse(age > 60, 'old', ifelse(age > 30, 'middle', 'young')))

dati$age <- as.factor(dati$age)

# JOB
table(dati$job)
job_tab <- data.frame(table(dati$job))
colnames(job_tab) <- c("job", "count")
ggplot(data=job_tab, aes(x=count, y=reorder(job,count),fill=job))+
  geom_bar(stat = 'identity', col="black")+
  labs(X=NULL,
       y=NULL,
       title="Customers' Job")
perc_top3<-sum(job_tab[1,2],job_tab[2,2],job_tab[10,2])/sum(job_tab[,2])
perc_top3#quasi il 65% delle osservazioni sono racchiuse in tre categorie, quindi tipologia di lavoro con poche osservazioni
#potrebbero sballarci le analisi quindi potrei decidere di aggruppare lavori simili per diminuirne la variabilità.
#faccio un analisi sugli unkown
a<-which(dati[,2]=="unknown")
dati<-dati[-a,]

#MARITAL
prop.table(table(dati$marital))
#plot(prop.table(table(dati[,c(3,17)])), col = c("blue", "red"), main = "output condizionato allo stato matrimoniale")
marital_tab <- data.frame(table(dati$marital))
colnames(marital_tab) <- c("marital", "count")
ggplot(data=marital_tab, aes(x=count, y=reorder(marital,count),fill=marital))+
  geom_bar(stat = 'identity', col="black")+
  labs(X=NULL,
       y=NULL,
       title="stato civile")
#decido di eliminare gli unknown perchè sono solo 71
b<-which(dati[,3]=="unknown")
dati<-dati[-b,]


#education
education_tab <- data.frame(table(dati$education))
colnames(education_tab) <- c("education", "count")
ggplot(data=education_tab, aes(x=count, y=reorder(education,count),fill=education))+
  geom_bar(stat = 'identity', col="black")+
  labs(X=NULL,
       y=NULL,
       title="livello d'istruzione")
for (i in 1:nrow(dati)){
  if(dati[i,4]=="illiterate") dati[i,4]<-"basic.4y"
}
c<-which(dati[,4]=="unknown")
sum(dati[,4]=="unknown")
dati<-dati[-c,]

#PREVIOUS

previous_tab <- data.frame(table(dati$previous))
colnames(previous_tab) <- c("previous", "count")
ggplot(data=previous_tab, aes(x=count, y=reorder(previous,count),fill=previous))+
  geom_bar(stat = 'identity', col="black")+
  labs(X=NULL,
       y=NULL,
       title="numero di contatti precedenti")

#uniamo le classi con numero >=2 in una categoria unica
for (i in 1:nrow(dati)) {
  if(dati$previous[i]>=2) dati$previous[i] <- 2
  
}
table(dati$previous)
previous_tab <- data.frame(table(dati$previous))
colnames(previous_tab) <- c("previous", "count")
ggplot(data=previous_tab, aes(x=count, y=reorder(previous,count),fill=previous))+
  geom_bar(stat = 'identity', col="black")+
  labs(X=NULL,
       y=NULL,
       title="numero di contatti precedenti")


#PDAYS
for (i in 1:nrow(dati)){
  if(dati[i,9]==999) dati[i,9]<-"no"
  else dati[i,9]<-"yes"
}
colnames(dati) <- c("age","job","marital","education","default",
                    "housing","loan","contact","pcontact","previous",
                    "poutcome","emp.var.rate","cons.price.idx",
                    "cons.conf.idx","euribor3m","nr.employed","y")  

#SUDDIVISIONE DATASET--------------------------------------------------
set.seed(123)
train_big_position <- sample(nrow(dati), nrow(dati)*0.8)
train_big <- dati[train_big_position,]
train_position <- sample(nrow(train_big),nrow(train_big)*0.75)

train <- train_big[train_position,]
vld <- train_big[-train_position,]
test <- dati[-train_big_position,]

#ANALISI ESPLORATIVA---------------------------
succ_tab <- data.frame(prop.table((table(train_big$y)))*100)
colnames(succ_tab) <- c("y","percentuale")
succ_tab


# AGE
ggplot(data = dati)+
  geom_bar(mapping = aes(x = factor(age,c("young","middle","old")), fill = y), position = "fill")

#JOB
CrossTable(train_big$job, train_big$y, prop.c = F, prop.t = F, prop.chisq = F)
#alcune osservazioni hanno molta più proporzione di yes rispetto ad unknown, la quale non ci 
#dà nessuna informazione sul lavoro, inoltre essendo poche osservazioni decido di toglierle.
job_y_tab <- data.frame(table(train$job, train$y))
colnames(job_y_tab) <- c("job","response","count")
ggplot(data=job_y_tab, aes(x=count,y=reorder(job,count), fill=response))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(X="Number of customers",
       y=NULL,
       title="Risultato campagna con tipo di lavoro")

#MARTIAL
mar_tab <- data.frame(table(train_big$marital, train_big$y))
colnames(mar_tab) <-c("marital_status","response","number_of_customers")

ggplot(data=mar_tab, aes(x=marital_status,y=number_of_customers, fill=response))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(X="customers",
       y=NULL,
       title="distribuzione di Marital ")

#EDUCATION
table(train_big$education)
ggplot(data=train_big)+
  geom_bar(mapping = aes(y=education,fill=y),position="fill")

#DEFAULT
table(train_big$default)

plot(prop.table(table(train_big[,c(5,17)])), col = c("blue", "red"), main = "output condizionato al livello d'istruzione")
e <- which(colnames(train_big)=="default")
train_big <- train_big[,-e]
train <- train[,-e]
vld <- vld[,-e]
test <- test[,-e]

#HOUSING
table(train_big$housing)
round(prop.table(table(train_big[, c(5, 16)])), 2)
prop.table(table(train_big$housing,train_big$y))*100
CrossTable(train_big$housing, train_big$y, prop.c = F, prop.t = F, prop.chisq = F)

ggplot(data = dati) +
  geom_bar(mapping = aes(x = housing, fill= y), position = "fill")
#la percentuale di gente che sottoscrive un term deposit è uguale tra quelli che hanno un mutuo per la casa e quelli che non lo hanno,
#quindi riteniamo che la variabile non sia utile a spiegare l'output e decidiamo di rimuoverla
e <- which(colnames(train_big)=="housing")
train_big <- train_big[,-e]
train <- train[,-e]
vld <- vld[,-e]
test <- test[,-e]

#LOAN
table(train_big$loan)
CrossTable(train_big$loan, train_big$y, prop.c = F, prop.t = F, prop.chisq = F)
round(prop.table(table(train_big[, c(5, 15)])), 2)

ggplot(data = dati) +
  geom_bar(mapping = aes(x = loan, fill= y), position = "fill")

#Lo stesso discorso fatto per la variabile housing vale anche per la variabile loan, che decidiamo quindi di rimuovere
e <- which(colnames(train_big)=="loan")
train_big <- train_big[,-e]
train <- train[,-e]
vld <- vld[,-e]
test <- test[,-e]


#CONTACT
table(train_big$contact)
CrossTable(train_big$contact, train_big$y, prop.c = F, prop.t = F, prop.chisq = F)
round(prop.table(table(train_big[, c(5, 14)])), 2)
ggplot(data=train_big)+
  geom_bar(mapping = aes(x=contact,fill=y),position="fill")
#La percentuale di persone contattate che rispondono positivamente al cellulare sembra essere più alta di quelle contattate al telefono fisso
#consideriamo dunque questa variabile importante ai fini della previsione

#PCONTACT
table(train_big$pcontact)
ggplot(data=train_big)+
  geom_bar(mapping = aes(x=pcontact,fill=y),position="fill")
#constatiamo che chi era già stato contattato è più propenso a dire di sì rispetto ai nuovi contatti

#PREVIOUS
ggplot(data=train_big)+
  geom_bar(mapping = aes(x=previous,fill=y),position="fill")
summary(train_big$previous)
#anche questa variabile insieme a pdays mostra che la maggior parte dei clienti non era mai stata contattata
#infatti media e mediana =0 inoltre ci conferma quello ipotizzato prima, ovvero chi è stato più contattato è più propenso a dire di sì
unique(train_big$previous)
table(train_big$previous)#controllo che ci fossero abbastanza variabili in 1 e 2

#POUTCOME
ggplot(data=train_big)+
  geom_bar(mapping = aes(x=poutcome,fill=y),position="dodge")
#la maggior parte dei clienti non è stata contattata prima di questa campagna

ggplot(data=train_big)+
  geom_bar(mapping = aes(x=poutcome,fill=y),position="fill")
#possiamo notare come quelli che avevano avuto un contatto precedente positivo
#sono più propensi a dire di sì

CrossTable(train_big$poutcome,train_big$y, prop.c = F, prop.t = F, prop.chisq = F)
#infatti chi ha già detto sì è più propenso a ridirlo.

#SOCIAL AND ECONOMIC CONTEXT VARIABLES 
train_big_eco <- train_big[,9:13]
Reco <- cor(train_big_eco)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Reco, method = 'color', type = 'upper', order = 'hclust', col = col(200), addCoef.col = 'black', diag = FALSE)
#La variabile euribor è altamente correlata sia con nr employed sia con emp var rate, perciò decidiamo di toglierla

e <- which(colnames(train_big)=="euribor3m")
train_big <- train_big[,-e]
train <- train[,-e]
vld <- vld[,-e]
test <- test[,-e]

#-------VERIFICA ASSUNZIONI-------
# Variabili di supporto
plot_box_all <-list()
variables <- colnames(train)[c(9:13)]

# Costruzione dei boxplot condizionati
for (i in variables){
  plot_box_all[[i]] <- ggplot(train[,c(9:13)], aes_string(x = "y", y = i, col = "y", fill = "y")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) 
  scale_fill_manual(values = c("blue", "red"))
}
do.call(grid.arrange, c(plot_box_all, nrow = 2))
#le variabili numeriche graficamente non sembrano condividere una varianza comune condizionata alle classi a parte cons.price.idx
train_numeric<-train[,c(9:13)]
# Costruzione del grafico per la valutazione delle covarianze
covEllipses(train_numeric[,1:(dim(train_numeric)[2]-1)], 
            factor(train_numeric$y), 
            fill = TRUE, 
            pooled = FALSE, 
            col = c("blue", "red"), 
            variables = c(1:3, (dim(train_numeric)[2]-1)), 
            fill.alpha = 0.05)

#verifico normalità delle variabili condizionate alla classe tramite la loro densità
# Variabile di supporto
plot_density <- list()
# Costruzione curve di densità condizionate rispetto alla classe
for(i in variables){
  plot_density[[i]] <- ggplot(train_numeric, aes_string(x = i, y = "..density..", col = "y")) + 
    geom_density(aes(y = ..density..)) + 
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = "none")
}
do.call(grid.arrange, c(plot_density, nrow = 3))
#non hanno distribuzione normale

# Normalizzazione #----------------
massimo<-vector()
minimo<-vector()

for( i in 1:(dim(train_numeric)[2]-1)) {
  massimo[i]<-max(train_numeric[,i])
  minimo[i]<-min(train_numeric[,i])
  for (j in 1:nrow(train_numeric)) {
    train_numeric[j,i]<-(train_numeric[j,i] - minimo[i])/(massimo[i] - minimo[i])
  }
}
train_norm <- train
train_norm[,c(9:12)]<-train_numeric[,1:4]

#vld
vld_numeric<-vld[,c(9:13)]
for( i in 1:4) {
  for (j in 1:nrow(vld_numeric)) {
    vld_numeric[j,i]<-(vld_numeric[j,i] - minimo[i])/(massimo[i] - minimo[i])
  }
}
vld_norm <- vld
vld_norm[,c(9:12)]<-vld_numeric[,1:4]

#train_big
train_big_numeric<- train_big[,c(9:13)]
for( i in 1:4) {
  for (j in 1:nrow(train_big_numeric)) {
    train_big_numeric[j,i]<-(train_big_numeric[j,i] - minimo[i])/(massimo[i] - minimo[i])
  }
}
train_big_norm <- train_big
train_big_norm[,c(9:12)]<-train_big_numeric[,1:4]

#test
test_numeric<- test[,c(9:13)]
for( i in 1:4) {
  for (j in 1:nrow(test_numeric)) {
    test_numeric[j,i]<-(test_numeric[j,i] - minimo[i])/(massimo[i] - minimo[i])
  }
}
test_norm <- test
test_norm[,c(9:12)]<-test_numeric[,1:4]
head(test_norm)

#METODI DI CLASSIFICAZIONE-----------------------------------------
#trasformiamo la variabile Y in 0 e 1
{for (j in 1:nrow(train_norm)) {
  if(train_norm$y[j]=="yes") train_norm$y[j] <- "1"
  else train_norm$y[j] <- "0"
}
  train_norm$y <- as.factor(train_norm$y)
  
  for (j in 1:nrow(train_big_norm)) {
    if(train_big_norm$y[j]=="yes") train_big_norm$y[j] <- "1"
    else train_big_norm$y[j] <- "0"
  }
  train_big_norm$y <- as.factor(train_big_norm$y)
  
  for (j in 1:nrow(test_norm)) {
    if(test_norm$y[j]=="yes") test_norm$y[j] <- "1"
    else test_norm$y[j] <- "0"
  }
  test_norm$y <- as.factor(test_norm$y)
  
  for (j in 1:nrow(vld_norm)) {
    if(vld_norm$y[j]=="yes") vld_norm$y[j] <- "1"
    else vld_norm$y[j] <- "0"
  }
  vld_norm$y <- as.factor(vld_norm$y)
}

#Reg logistica###################################################################################################

#Come primo modello scegliamo di testare il Modello Logistico e di applicare una selezione stepwise basata su AIC
model_logit <- glm(y ~., data = train_norm, family = binomial)
summary(model_logit)
step.model <- stepAIC(model_logit, direction = "both", trace = FALSE)
summary(step.model)

anova(step.model, test="Chisq")

# Analisi dei punti influenti
outlierTest(step.model)
which(hatvalues(step.model)>15*mean(hatvalues(step.model)))
influencePlot(step.model)
rstudent(step.model)[19061]
hatvalues(step.model)[19061]

# Nuova stima del modello di regressione logistica eliminando prima le osservazioni considerate influenti
train_1 <- train_norm[-c(37908,37291,19061,22193,39724,27833),]

model_logit2 <- glm(y ~., data = train_1, family = binomial)
step.model2 <- stepAIC(model_logit2, direction = "both", trace = FALSE)

summary(step.model2)


#calcolo delle probabilità a posteriori
pred_logit_vld <- predict(step.model2, newdata = vld_norm[, -13], type = "response")
# Trasformazioni probabilità in classe, con soglia 0.5
pred_logit_class_vld1 <- ifelse (pred_logit_vld > 0.5, 1, 0) #^^^
confusionMatrix(factor(as.vector(pred_logit_class_vld1)),factor(vld_norm[, 13]),positive = "1")
#sum(vld_norm$y==1) #844 numero di successi nel validation 
pred_logit_class_vld2 <- ifelse (pred_logit_vld > 0.13, 1, 0) #^^^
confusionMatrix(factor(as.vector(pred_logit_class_vld2)),factor(vld_norm[, 13]),positive = "1")

# Calcolo ROC per regressione logistica
pred_roclogit_vld <- prediction(pred_logit_vld, vld_norm$y) #^^^
perf_logit_vld <- performance(pred_roclogit_vld,"tpr","fpr")

plot(perf_logit_vld, colworize = TRUE, main = "Roc Curve - Reg. Logistica (vld set)")

#notiamo che utilizzando un valore soglia di 0.13 otteniamo un grosso miglioramento
#per quanto riguarda il valore di sensitivity (a discapito della specificity che si riduce ma non di molto)


#---- train big e test
model_logit_final <- glm(y~job + education + contact + pcontact + poutcome + 
                           emp.var.rate + cons.price.idx + cons.conf.idx, 
                         family = binomial, data = train_big_norm)

summary(model_logit_final)
influencePlot(model_logit_final)
nrow(train_big_norm)

train_big_1 <- train_big_norm[-c(21762,37291,22193,37908,27833,40413),]
# Modello finale
model_logit_final <- glm(y~job + education + contact + pcontact + poutcome + 
                           emp.var.rate + cons.price.idx + cons.conf.idx, family = binomial, data = train_big_1)
summary(model_logit_final)

{pred_logit <- predict(model_logit_final, newdata = test_norm[,-13], type = "response")
  pred_logit_class <- ifelse (pred_logit> 0.13, 1, 0) #utilizziamo il valore soglia determinato nella fase train-validation
  confusionMatrix( factor(as.vector(pred_logit_class)),factor(test_norm[, 13]),positive = "1")
}

# Calcolo train_big(train+vld) error
pred_logit_train_big <- predict(model_logit_final, type = "response")
pred_logit_class_train_big <- ifelse (pred_logit_train_big > 0.13, 1, 0) 
cm <- confusionMatrix(factor(as.vector(pred_logit_class_train_big)),factor(train_big_1[, 13]),positive = "1")
(accuratezza_train_big <- cm$overall["Accuracy"])
train_big_error <- 1 - accuratezza_train_big
names(train_big_error) <- "Training Error"
train_big_error#0.1922868

# Calcolo test error
cm1 <- confusionMatrix(factor(as.vector(pred_logit_class)),factor(test_norm[, 13]),positive = "1")
(accuratezza_test <- cm1$overall["Accuracy"])
test_error <- 1 - accuratezza_test
names(test_error) <- "Test Error"
test_error#0.185738

# Calcolo ROC per regressione logistica
pred_roclogit <- prediction(pred_logit, test_norm$y)
perf_logit <- performance(pred_roclogit,"tpr","fpr")

plot(perf_logit, colworize = TRUE, main = "Roc Curve - Reg. Logistica (test set)")

#KNN##################################################################################

ktrain <- train_norm
kvld <- vld_norm
ktrain_big <- train_big_norm
ktest <- test_norm

ktrain$education <- as.factor(ktrain$education)
ktrain$education <- as.numeric(ktrain$education)
ktrain <- ktrain[, -2]#tolgo job
ktrain <- dummy_cols(ktrain)
ktrain <- ktrain[, -c(1, 2, 4, 5, 7, 26, 27)]
x_ktrain <- ktrain[, c(1:6, 8:20)]
y_ktrain <- ktrain[, 7]

ktrain_big$education <- as.factor(ktrain_big$education)
ktrain_big$education <- as.numeric(ktrain_big$education)
ktrain_big <- ktrain_big[, -2]
ktrain_big <- dummy_cols(ktrain_big)
ktrain_big <- ktrain_big[, -c(1, 2, 4, 5, 7, 26, 27)]
x_ktrain_big <- ktrain_big[, c(1:6, 8:20)]
y_ktrain_big <- ktrain_big[, 7]

kvld$education <- as.factor(kvld$education)
kvld$education <- as.numeric(kvld$education)
kvld <- kvld[, -2]
kvld <- dummy_cols(kvld)
kvld <- kvld[, -c(1, 2, 4, 5, 7, 26, 27)]
x_kvld <- kvld[, c(1:6, 8:20)]
y_kvld <- kvld[, 7]

ktest$education <- as.factor(ktest$education)
ktest$education <- as.numeric(ktest$education)
ktest <- ktest[, -2]
ktest <- dummy_cols(ktest)
ktest <- ktest[, -c(1, 2, 4, 5, 7, 26, 27)]
x_ktest <- ktest[, c(1:6, 8:20)]
y_ktest <- ktest[, 7]

#funzione errore rate
calc_class_err = function(actual, predicted) { mean(actual != predicted) }

k_to_try = 1:20
err_k = rep(x = 0, times = 20)
for (i in seq_along(k_to_try)){
  pred = knn(train = x_ktrain, test = x_kvld, cl = y_ktrain, k = k_to_try[i])
  err_k[i] <- calc_class_err(actual = y_kvld, predicted = pred)
}

min(err_k)
min(which(err_k == min(err_k)))#20
#scelgo k minore a parità di errore, siccome ho una percentuale di si molto inferiore rispetto ai no
calc_class_err(actual = y_kvld, predicted = knn(train = x_ktrain, test = x_kvld, cl = y_ktrain, k = 20))

plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "classification error", 
     main = "(train-vld) Error Rate vs Neighbors")
abline(h = min(err_k), col = "darkorange", lty = 3)# Minimum error


# Previsione con k migliore
pred = knn(train = ktrain[,-7],
           test = kvld[,-7], cl = ktrain$y,
           k = 20, prob = TRUE)
# confusion matrix
conf_matrix = confusionMatrix(pred,factor(kvld$y),positive = "1")
conf_matrix

prob_knn1 <- attributes(pred)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn2 <- 2*ifelse(pred == "0", 1-prob_knn1, prob_knn1) - 1
pred_rocknn <- prediction(prob_knn2, kvld$y)
perf_knn<- performance(pred_rocknn,"tpr","fpr")

plot(perf_knn, colorize = TRUE, main = "20-NN")


#Calcolo validation error
validation_error <- 1-conf_matrix$overall["Accuracy"]
names(validation_error) <- "Validation Error"
validation_error


#----------------- train big e test
#prev
pred2 = knn(train = ktrain_big[,-7],
            test = ktrain_big[,-7], cl = ktrain_big$y,
            k = 20, prob = TRUE)


# confusion matrix
conf_matrix2 = confusionMatrix(pred2,factor(ktrain_big$y),positive = "1")
conf_matrix2
#Calcolo training Error (train+validation)
training_error <- 1-conf_matrix2$overall["Accuracy"]
names(training_error) <- "Training Error"
training_error#0.1003445

#prev
pred3 = knn(train = ktrain_big[,-7],
            test = ktest[,-7], cl = ktrain_big$y,
            k = 20, prob = TRUE)
# confusion matrix
conf_matrix3 = confusionMatrix(pred3,factor(ktest$y),positive = "1")
conf_matrix3

#Calcolo test error
test_error <- 1-conf_matrix3$overall["Accuracy"]
names(test_error) <- "Test Error"
test_error#0.09707871





