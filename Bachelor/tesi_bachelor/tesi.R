library(ggcorrplot)
library(readr)
library(car)
library(gridExtra)
data_mensile<- read.table("Europe_3_Factors.txt",skip = 7, header=F, col.names=c("date","Mkt-RF","SMB","HML","RF"), nrows =  393 ,strip.white = TRUE)
data_annuale<- read.table("Europe_3_Factors.txt",skip = 405, header=F, col.names=c("date","Mkt-RF","SMB","HML","RF"), nrows =  32 ,strip.white = TRUE)
numero_titoli<-read.table("Europe_6_Portfolios_ME_BE-ME.txt", skip = 896, header = F,col.names =c("date","SG","SN", "SV","BG","BN","BV"), nrows = 393, strip.white = T)
average_size<-read.table("Europe_6_Portfolios_ME_BE-ME.txt", skip = 1295, header = F,col.names =c("date","SG","SN", "SV","BG","BN","BV"), nrows = 393, strip.white = T)
apply(numero_titoli, 2, function(x) range(x))
apply(numero_titoli, 2, function(x) mean(x))

apply(average_size, 2, function(x) range(x))
apply(average_size, 2, function(x) mean(x))

data_mensile_test<-data_mensile[355:393,]
data_mensile<-data_mensile[1:354,]
correlazioni<-cor(data_mensile[,-1])
library(ggcorrplot)
ggcorrplot(correlazioni, hc.order = T, type="lower", lab=T)

portafoglio_montly_weighted<- read_csv("Europe_6_Portfolios_ME_BE-ME.csv", col_names = T, skip = 20, n_max = 393)
colnames(portafoglio_montly_weighted)<-c("data", "SG","SN","SV", "BG","BN","BV")

prova<-portafoglio_montly_weighted[,-1]-data_mensile[,2]
apply(prova,2, function(x) mean(x))# media small=0.2221657
#media big= 0.2315913
media1<- data.frame(
  Portafoglio = c("Low","Neutral","Big","Low","Neutral","Big"),
  Media = apply(portafoglio_montly_weighted[,-1],2, function(x) mean(x)),
  Gruppo = c("Small", "Small", "Small", "Big", "Big", "Big")
)
ggplot(media1, aes(x = Portafoglio, y = Media, color = Gruppo, group = Gruppo)) +
  geom_line(size = 1) +
  labs(x = "B/M", y = "Media", color = "Size") +
  theme_minimal()

apply(prova,2, function(x) sd(x))

sd1<- data.frame(
  Portafoglio = c("Low","Neutral","Big","Low","Neutral","Big"),
  Media = apply(portafoglio_montly_weighted[,-1],2, function(x) sd(x)),
  Gruppo = c("Small", "Small", "Small", "Big", "Big", "Big")
)
ggplot(sd1, aes(x = Portafoglio, y = Media, color = Gruppo, group = Gruppo)) +
  geom_line(size = 1) +
  labs(x = "B/M", y = "Media", color = "Size") +
  theme_minimal()
portafoglio_montly_weighted_test<-portafoglio_montly_weighted[355:393,]
portafoglio_montly_weighted<-portafoglio_montly_weighted[1:354,]

portafoglio_montly_equal<- read_csv("Europe_6_Portfolios_ME_BE-ME.csv", col_names = T, skip = 418, n_max = 393)
colnames(portafoglio_montly_equal)<-c("data", "SG","SN","SV", "BG","BN","BV")
prova_eq<-portafoglio_montly_equal[,-1]-data_mensile[,2]
apply(prova_eq,2, function(x) mean(x))
apply(prova_eq,2, function(x) sd(x))
portafoglio_montly_equal_test<-portafoglio_montly_equal[355:393,]
portafoglio_montly_equal<-portafoglio_montly_equal[1:354,]

portafoglio_annual_weighted<- read_csv("Europe_6_Portfolios_ME_BE-ME.csv", col_names = T, skip = 816, n_max = 32)
colnames(portafoglio_annual_weighted)<-c("data", "SG","SN","SV", "BG","BN","BV")
apply(portafoglio_annual_weighted[,-1],2, function(x) mean(x))
apply(portafoglio_annual_weighted[,-1],2, function(x) sd(x))

portafoglio_annual_equal<- read_csv("Europe_6_Portfolios_ME_BE-ME.csv", col_names = T, skip = 853, n_max = 32)
colnames(portafoglio_annual_equal)<-c("data", "SG","SN","SV", "BG","BN","BV")
apply(portafoglio_annual_equal[,-1],2, function(x) mean(x))
apply(portafoglio_annual_equal[,-1],2, function(x) sd(x))

#regressione mensile weighted
#------1-----
data_reg1<-data_mensile[,-1]
data_reg1<-cbind(portafoglio_montly_weighted$SG,data_reg1)
colnames(data_reg1)<-c("Ri","Mkt.RF","SMB","HML","RF")


mod1<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg1) #mod1 è con Sg ovvero small e growth(low B/M)
summary(mod1)

#diagnostica residui del modello
par(mfrow=c(2,2))
plot(mod1)
# Creazione del dataframe dei residui e dei valori previsti
residuals_df <- data.frame(Residuals = mod1$residuals, Predicted = mod1$fitted.values)

# Plot dei residui rispetto ai valori previsti
plot1<-ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valori previsti", y = "Residui") +
  theme_minimal()
residuals_df2 <- data.frame(Residuals = mod1$residuals, Order = 1:length(mod1$residuals))

# Plot dei residui contro l'ordine dei dati
plot2<-ggplot(residuals_df2, aes(x = Order, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Ordine dei dati", y = "Residui") +
  theme_minimal()
# Calcola i residui internamente studentizzati
residuals <- rstandard(mod1)

# Crea un data frame con i residui internamente studentizzati e i valori predetti
data <- data.frame(fitted = fitted(mod1), residuals = residuals)

# Crea il plot
plot3<-ggplot(data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Valori predetti", y = "Residui internamente studentizzati") +
  theme_minimal()

std_resid <- resid(mod1) / sqrt(sd(resid(mod1))^2 * (1 - hatvalues(mod1)))

# Calcolo dei quantili teorici
quantile_theoretical <- qnorm(ppoints(length(std_resid)))

# Creazione del dataframe per il QQ plot
qq_data <- data.frame(QuantileTeorico = quantile_theoretical,
                      ResiduoStudentizzato = std_resid)

# Creazione del QQ plot con ggplot2
plot4<-ggplot(qq_data, aes(sample = ResiduoStudentizzato)) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Quantili teorici", y = "Residui studentizzati") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
#intercetta=0
residui1 <- residuals(mod1)
V <- vcov(mod1)
Vn<- V * nrow(data_reg1)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod1)) %*% Vn_inv %*% coef(mod1) / (1 + t(coef(mod1)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod1)), ncol = 1))
df_numeratore <- length(coef(mod1))
df_denominatore <- nrow(data_reg1) - length(coef(mod1)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

rendimenti_3f<-cbind(data_reg1[,1]-data_reg1[,5],data_reg2[,1]-data_reg2[,5],data_reg3[,1]-data_reg3[,5],data_reg4[,1]-data_reg4[,5],
      data_reg5[,1]-data_reg5[,5],data_reg6[,1]-data_reg6[,5])
library(GRS.test)
GRS.test(rendimenti_3f, data_reg1[,2:4])
#------2-----
data_reg2<-data_mensile[,-1]
data_reg2<-cbind(portafoglio_montly_weighted$SN,data_reg2)
colnames(data_reg2)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod2<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg2) #mod1 è con SN ovvero small e neutral B/M
summary(mod2)

#diagnostica residui del modello

plot(mod2)

#intercetta=0
residui2 <- residuals(mod2)
V <- vcov(mod2)
Vn<- V * nrow(data_reg2)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod2)) %*% Vn_inv %*% coef(mod2) / (1 + t(coef(mod2)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod2)), ncol = 1))
df_numeratore <- length(coef(mod2))
df_denominatore <- nrow(data_reg2) - length(coef(mod2)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0



#------3-----
data_reg3<-data_mensile[,-1]
data_reg3<-cbind(portafoglio_montly_weighted$SV,data_reg3)
colnames(data_reg3)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod3<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg3) #mod1 è con SV ovvero small e value (high B/M)
summary(mod3)

#diagnostica residui del modello

plot(mod3)

#intercetta=0
residui3 <- residuals(mod3)
V <- vcov(mod3)
Vn<- V * nrow(data_reg3)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod3)) %*% Vn_inv %*% coef(mod3) / (1 + t(coef(mod3)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod3)), ncol = 1))
df_numeratore <- length(coef(mod3))
df_denominatore <- nrow(data_reg3) - length(coef(mod3)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

#------4-----
data_reg4<-data_mensile[,-1]
data_reg4<-cbind(portafoglio_montly_weighted$BG,data_reg4)
colnames(data_reg4)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod4<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg4) # è con BG ovvero big e low B/M
summary(mod4)

#diagnostica residui del modello

plot(mod4)

#intercetta=0
residui4 <- residuals(mod4)
V <- vcov(mod4)
Vn<- V * nrow(data_reg4)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod4)) %*% Vn_inv %*% coef(mod4) / (1 + t(coef(mod4)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod4)), ncol = 1))
df_numeratore <- length(coef(mod4))
df_denominatore <- nrow(data_reg4) - length(coef(mod4)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

#------5-----
data_reg5<-data_mensile[,-1]
data_reg5<-cbind(portafoglio_montly_weighted$BN,data_reg5)
colnames(data_reg5)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod5<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg5) # è con BN ovvero big e neutral B/M
summary(mod5)

#diagnostica residui del modello

plot(mod5)

#intercetta=0
residui5 <- residuals(mod5)
V <- vcov(mod5)
Vn<- V * nrow(data_reg5)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod5)) %*% Vn_inv %*% coef(mod5) / (1 + t(coef(mod5)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod5)), ncol = 1))
df_numeratore <- length(coef(mod5))
df_denominatore <- nrow(data_reg5) - length(coef(mod5)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

#------6-----
data_reg6<-data_mensile[,-1]
data_reg6<-cbind(portafoglio_montly_weighted$BV,data_reg6)
colnames(data_reg6)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod6<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg6) #m è con BV ovvero big e high B/M
summary(mod6)

#diagnostica residui del modello

plot(mod6)
par(mfrow=c(1,1))

#intercetta=0
residui6 <- residuals(mod6)
V <- vcov(mod6)
Vn<- V * nrow(data_reg6)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod6)) %*% Vn_inv %*% coef(mod6) / (1 + t(coef(mod6)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod6)), ncol = 1))
df_numeratore <- length(coef(mod6))
df_denominatore <- nrow(data_reg6) - length(coef(mod6)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

#-------------------------------------------------------

#regressione mensile equal
#------1-----
data_reg1_eq<-data_mensile[,-1]
data_reg1_eq<-cbind(portafoglio_montly_equal$SG,data_reg1_eq)
colnames(data_reg1_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")


mod1_eq<-lm(Ri-RF~(Mkt.RF)+SMB+HML, data = data_reg1_eq) #mod1 è con Sg ovvero small e growth(low B/M)
summary(mod1_eq)

#diagnostica residui del modello
par(mfrow=c(2,2))
plot(mod1_eq)

#intercetta=0
residui1 <- residuals(mod1_eq)
V <- vcov(mod1_eq)
Vn<- V * nrow(data_reg1_eq)
Vn_inv <- solve(Vn)
GRS <- t(coef(mod1_eq)) %*% Vn_inv %*% coef(mod1_eq) / (1 + t(coef(mod1_eq)) %*% Vn_inv %*% matrix(1, nrow = length(coef(mod1_eq)), ncol = 1))
df_numeratore <- length(coef(mod1_eq))
df_denominatore <- nrow(data_reg1_eq) - length(coef(mod1_eq)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

#------2-----
data_reg2_eq<-data_mensile[,-1]
data_reg2_eq<-cbind(portafoglio_montly_equal$SN,data_reg2_eq)
colnames(data_reg2_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod2_eq<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg2_eq) #mod1 è con SN ovvero small e neutral B/M
summary(mod2_eq)

#diagnostica residui del modello

plot(mod2_eq)

#analisi robustezza

#------3-----
data_reg3_eq<-data_mensile[,-1]
data_reg3_eq<-cbind(portafoglio_montly_equal$SV,data_reg3_eq)
colnames(data_reg3_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod3_eq<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg3_eq) #mod1 è con SV ovvero small e value (high B/M)
summary(mod3_eq)

#diagnostica residui del modello

plot(mod3_eq)

#analisi robustezza

#------4-----
data_reg4_eq<-data_mensile[,-1]
data_reg4_eq<-cbind(portafoglio_montly_equal$BG,data_reg4_eq)
colnames(data_reg4_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod4_eq<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg4_eq) # è con BG ovvero big e low B/M
summary(mod4_eq)

#diagnostica residui del modello

plot(mod4_eq)

#analisi robustezza

#------5-----
data_reg5_eq<-data_mensile[,-1]
data_reg5_eq<-cbind(portafoglio_montly_equal$BN,data_reg5_eq)
colnames(data_reg5_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod5_eq<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg5_eq) # è con BN ovvero big e neutral B/M
summary(mod5_eq)

#diagnostica residui del modello

plot(mod5_eq)

#analisi robustezza

#------6-----
data_reg6_eq<-data_mensile[,-1]
data_reg6_eq<-cbind(portafoglio_montly_equal$BV,data_reg6_eq)
colnames(data_reg6_eq)<-c("Ri","Mkt.RF","SMB","HML","RF")

mod6_eq<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg6_eq) #m è con BV ovvero big e high B/M
summary(mod6_eq)

#diagnostica residui del modello

plot(mod6_eq)
par(mfrow=c(1,1))

#analisi robustezza


#leverage
#---------
h.ii<-hatvalues(mod6_eq)
#soglia=2(k+1)/n Qui si ha n=34,k=6
soglia<-2*4/393;soglia
h.ii[h.ii>soglia]

#outlier
outlierTest(mod6_eq)
influenceIndexPlot(mod6_eq)
influencePlot(mod6_eq)

mod6_eq2<-lm(Ri-RF~Mkt.RF+SMB+SMB, data = data_reg6_eq[-c(28,154,225,226,259),]) #cambia davvero poco
summary(mod6_eq2)


#CAPM

covarianze<-cov(portafoglio_montly_weighted[,-1],data_mensile$Mkt.RF)
varianza<-var(data_mensile$Mkt.RF)
beta<-covarianze/varianza

modCAPM1<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg1)
summary(modCAPM1)
#intercetta=0
residui1 <- residuals(modCAPM1)
V <- vcov(modCAPM1)
Vn<- V * nrow(data_reg1)
Vn_inv <- solve(Vn)
GRS <- t(coef(modCAPM1)) %*% Vn_inv %*% coef(modCAPM1) / (1 + t(coef(modCAPM1)) %*% Vn_inv %*% matrix(1, nrow = length(coef(modCAPM1)), ncol = 1))
df_numeratore <- length(coef(modCAPM1))
df_denominatore <- nrow(data_reg1) - length(coef(modCAPM1)) - 1
p_value <- 1 - pf(GRS, df_numeratore, df_denominatore)
p_value#>0.05 quindi accetto l'ipotesi che l'intercetta =0

modCAPM2<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg2)
summary(modCAPM2)

modCAPM3<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg3)
summary(modCAPM3)

modCAPM4<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg4)
summary(modCAPM4)

modCAPM5<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg5)
summary(modCAPM5)

modCAPM6<-lm(Ri-RF~(Mkt.RF-RF), data = data_reg6)
summary(modCAPM6)

#cerchiamo di verificare quanto siano importanti i fattori comparando le previsioni 
#solo SMB
mod1SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg1) 
summary(mod1SMB)

mod2SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg2) 
summary(mod2SMB)

mod3SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg3) 
summary(mod3SMB)

mod4SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg4) 
summary(mod4SMB)

mod5SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg5) 
summary(mod5SMB)

mod6SMB<-lm(Ri-RF~(Mkt.RF-RF)+SMB, data = data_reg6) 
summary(mod6SMB)

#solo HML
mod1HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg1) 
summary(mod1HML)

mod2HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg2) 
summary(mod2HML)

mod3HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg3) 
summary(mod3HML)

mod4HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg4) 
summary(mod4HML)

mod5HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg5) 
summary(mod5HML)

mod6HML<-lm(Ri-RF~(Mkt.RF-RF)+HML, data = data_reg6) 
summary(mod6HML)

#PREVISIONI SUL TEST
#1
data_reg1_test<-data_mensile_test[,-1]
data_reg1_test<-cbind(portafoglio_montly_weighted_test$SG,data_reg1_test)
colnames(data_reg1_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod1<-predict(mod1,data_reg1_test)
prediction_mod1SMB<-predict(mod1SMB,data_reg1_test)
prediction_mod1HML<-predict(mod1HML,data_reg1_test)
prediction_mod1CAPM<-predict(modCAPM1,data_reg1_test)
rendimenti_effettivi<-data_reg1_test$Ri-data_reg1_test$RF
periodo <- as.vector(portafoglio_montly_weighted_test$data)

library(zoo)
periodo_date <- as.yearmon(as.character(periodo), format = "%Y%m")

periodo_mese_anno <- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod1, prediction_mod1SMB, prediction_mod1HML, prediction_mod1CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod1","prediction_mod1CAPM",key= "tipo", value="valore")
str(df)
confronto1<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse1 <- mean((rendimenti_effettivi - prediction_mod1)^2))
(mseSMB<-mean((rendimenti_effettivi-prediction_mod1SMB)^2))
(mseHML<-mean((rendimenti_effettivi-prediction_mod1HML)^2))
(mseCAPM<-mean((rendimenti_effettivi-prediction_mod1CAPM)^2))

#2
data_reg2_test<-data_mensile_test[,-1]
data_reg2_test<-cbind(portafoglio_montly_weighted_test$SN,data_reg2_test)
colnames(data_reg2_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod2<-predict(mod2,data_reg2_test)
prediction_mod2SMB<-predict(mod2SMB,data_reg2_test)
prediction_mod2HML<-predict(mod2HML,data_reg2_test)
prediction_mod2CAPM<-predict(modCAPM2,data_reg2_test)
rendimenti_effettivi2<-data_reg2_test$Ri-data_reg2_test$RF
periodo2 <- as.vector(portafoglio_montly_weighted_test$data)

periodo_date<- as.yearmon(as.character(periodo2), format = "%Y%m")

periodo_mese_anno<- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod2, prediction_mod2CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod2","prediction_mod2CAPM",key= "tipo", value="valore")
confronto2<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse2 <- mean((rendimenti_effettivi2 - prediction_mod2)^2))
(mseSMB2<-mean((rendimenti_effettivi2-prediction_mod2SMB)^2))
(mseHML2<-mean((rendimenti_effettivi2-prediction_mod2HML)^2))
(mseCAPM2<-mean((rendimenti_effettivi2-prediction_mod2CAPM)^2))

#3
data_reg3_test<-data_mensile_test[,-1]
data_reg3_test<-cbind(portafoglio_montly_weighted_test$SV,data_reg3_test)
colnames(data_reg3_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod3<-predict(mod3,data_reg3_test)
prediction_mod3SMB<-predict(mod3SMB,data_reg3_test)
prediction_mod3HML<-predict(mod3HML,data_reg3_test)
prediction_mod3CAPM<-predict(modCAPM3,data_reg3_test)
rendimenti_effettivi3<-data_reg3_test$Ri-data_reg3_test$RF
periodo3 <- as.vector(portafoglio_montly_weighted_test$data)

periodo_date<- as.yearmon(as.character(periodo3), format = "%Y%m")

periodo_mese_anno<- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod3, prediction_mod3CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod3","prediction_mod3CAPM",key= "tipo", value="valore")
confronto3<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse3 <- mean((rendimenti_effettivi3 - prediction_mod3)^2))
(mseSMB3<-mean((rendimenti_effettivi3-prediction_mod3SMB)^2))
(mseHML3<-mean((rendimenti_effettivi3-prediction_mod3HML)^2))
(mseCAPM3<-mean((rendimenti_effettivi3-prediction_mod3CAPM)^2))

#4
data_reg4_test<-data_mensile_test[,-1]
data_reg4_test<-cbind(portafoglio_montly_weighted_test$BG,data_reg4_test)
colnames(data_reg4_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod4<-predict(mod4,data_reg4_test)
prediction_mod4SMB<-predict(mod4SMB,data_reg4_test)
prediction_mod4HML<-predict(mod4HML,data_reg4_test)
prediction_mod4CAPM<-predict(modCAPM4,data_reg4_test)
rendimenti_effettivi4<-data_reg4_test$Ri-data_reg4_test$RF
periodo4 <- as.vector(portafoglio_montly_weighted_test$data)

periodo_date<- as.yearmon(as.character(periodo4), format = "%Y%m")

periodo_mese_anno<- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod4, prediction_mod4CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod4","prediction_mod4CAPM",key= "tipo", value="valore")
confronto4<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse4 <- mean((rendimenti_effettivi4 - prediction_mod4)^2))
(mseSMB4<-mean((rendimenti_effettivi4-prediction_mod4SMB)^2))
(mseHML4<-mean((rendimenti_effettivi4-prediction_mod4HML)^2))
(mseCAPM4<-mean((rendimenti_effettivi4-prediction_mod4CAPM)^2))

#5
data_reg5_test<-data_mensile_test[,-1]
data_reg5_test<-cbind(portafoglio_montly_weighted_test$BN,data_reg5_test)
colnames(data_reg5_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod5<-predict(mod5,data_reg5_test)
prediction_mod5SMB<-predict(mod5SMB,data_reg5_test)
prediction_mod5HML<-predict(mod5HML,data_reg5_test)
prediction_mod5CAPM<-predict(modCAPM5,data_reg5_test)
rendimenti_effettivi5<-data_reg5_test$Ri-data_reg5_test$RF
periodo5 <- as.vector(portafoglio_montly_weighted_test$data)

periodo_date<- as.yearmon(as.character(periodo5), format = "%Y%m")

periodo_mese_anno<- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod5, prediction_mod5CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod5","prediction_mod5CAPM",key= "tipo", value="valore")
confronto5<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse5 <- mean((rendimenti_effettivi5 - prediction_mod5)^2))
(mseSMB5<-mean((rendimenti_effettivi5-prediction_mod5SMB)^2))
(mseHML5<-mean((rendimenti_effettivi5-prediction_mod5HML)^2))
(mseCAPM5<-mean((rendimenti_effettivi5-prediction_mod5CAPM)^2))

#6
data_reg6_test<-data_mensile_test[,-1]
data_reg6_test<-cbind(portafoglio_montly_weighted_test$BV,data_reg6_test)
colnames(data_reg6_test)<-c("Ri","Mkt.RF","SMB","HML","RF")

prediction_mod6<-predict(mod6,data_reg6_test)
prediction_mod6SMB<-predict(mod6SMB,data_reg6_test)
prediction_mod6HML<-predict(mod6HML,data_reg6_test)
prediction_mod6CAPM<-predict(modCAPM6,data_reg6_test)
rendimenti_effettivi6<-data_reg6_test$Ri-data_reg6_test$RF
periodo6 <- as.vector(portafoglio_montly_weighted_test$data)

periodo_date<- as.yearmon(as.character(periodo6), format = "%Y%m")

periodo_mese_anno<- format(periodo_date, format = "%Y-%m")

df<-data.frame(periodo_mese_anno,rendimenti_effettivi, prediction_mod6, prediction_mod6CAPM)
df2<-df[27:39,]%>%
  gather("rendimenti_effettivi","prediction_mod6","prediction_mod6CAPM",key= "tipo", value="valore")
confronto6<-ggplot(data = df2, aes(x = periodo_mese_anno, y = valore, group = tipo, color = tipo)) +
  geom_line() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.9, 0.20),  # Posizione della legenda (basso a destra)
    legend.background = element_rect(fill = "transparent"),  # Sfondo trasparente per la legenda
    legend.text = element_text(size = 8)  # Dimensione del testo della legenda
  ) +
  labs(x = "", y = "Ri-RF", color = "Legenda") +
  scale_color_manual(
    labels = c("three-factor model", "CAPM", "Rendimenti effettivi"),
    values = c("blue", "red", "black")
  )
(mse6 <- mean((rendimenti_effettivi6 - prediction_mod6)^2))
(mseSMB6<-mean((rendimenti_effettivi6-prediction_mod6SMB)^2))
(mseHML6<-mean((rendimenti_effettivi6-prediction_mod6HML)^2))
(mseCAPM6<-mean((rendimenti_effettivi6-prediction_mod6CAPM)^2))


#analisi sensibilità periodo regressione mensile equal
#------1-----

mod1_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg1_test) 
summary(mod1_test)

#------2-----
mod2_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg2_test) 
summary(mod2_test)

#------3-----
mod3_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg3_test) 
summary(mod3_test)

#------4-----
mod4_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg4_test) 
summary(mod4_test)

#------5-----
mod5_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg5_test) 
summary(mod5_test)

#------6-----
mod6_test<-lm(Ri-RF~(Mkt.RF-RF)+SMB+HML, data = data_reg6_test) 
summary(mod6_test)


confronto1
confronto2
confronto3
confronto4
confronto5
confronto6

library(GRS.test)
rendimenti<-cbind(data_reg1[,1]-data_reg1[,5],data_reg2[,1]-data_reg2[,5],data_reg3[,1]-data_reg3[,5],data_reg4[,1]-data_reg4[,5],
                     data_reg5[,1]-data_reg5[,5],data_reg6[,1]-data_reg6[,5])
GRS.test(rendimenti, data_reg1[,2:4])
GRS.test(rendimenti, data_reg1[,2])
