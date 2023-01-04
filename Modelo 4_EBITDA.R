###------------------------ Modelo EBITDA ---------------- ####
# Preparación y carga de datos:
rm(list=ls())
library(readxl)
library(ggplot2)
datos_0 <- read_excel("Base de datos_B.xlsx")
attach(datos_0)

# Transformación de datos:
library(dplyr)
datos_00 <- datos_0 %>%
  summarise(
    EBITDA.plus=EBITDA,
    gas_waha.neg=-gas_waha)

library(dplyr)
datos_1 <- datos_0 %>%
  summarise(
    EBITDA.d=diff(log(EBITDA)),
    gas_waha.d=-diff(log(gas_waha)))
datos_1$gas_waha.d[is.na(datos_1$gas_waha.d)] <- mean(datos_1$gas_waha.d, na.rm = TRUE)  
#################################################################### Niveles.
M4_EBITDA.d0 <- lm(EBITDA.plus~
           gas_waha.neg-1,
         data=datos_00)
summary(M4_EBITDA.d0)

################################################################# diff(log(x)
M4_EBITDA.d1 <- lm(EBITDA.d~
           gas_waha.d-1,
         data=datos_1)
summary(M4_EBITDA.d1)

################################################################# cointegración
y0_1 <- (EBITDA)
x0_1 <- (gas_waha)
library(urca)
jotest=ca.jo(data.frame(y0_1,x0_1), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

################################################################# Raíz unitaria
a=diff(log(EBITDA))
b=diff(log(gas_waha))
b[is.na(b)] <- mean(b, na.rm = TRUE)
library(tseries)
adf.test(a)
pp.test(a)

library(tseries)
adf.test(b)
pp.test(b)

################################################################# Gráfica
datos_1 %>%
  ggplot(aes(x=gas_waha.d,
             y=EBITDA.d))+
  theme_bw()+
  labs(title="EBITDA.d~gas_waha.d",
       y=expression(paste("diff(log(EBITDA))")),
       x=expression(paste("diff(log(gas_waha))")))+
  geom_point(col="blue",
             size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(labels=scales::percent,limits=c(-2,2))+
  scale_y_continuous(labels=scales::percent,limits=c(-2,2))+
  geom_smooth(method="lm",
              se=F,
              fullrange=T,
              col="red")

###------------------------ Pruebas diagnostico: EBITDA ------------------- ####
#   Pruebas                       Modelo +     p-values

#1. Prueba de media cero:         Cumple.   
#                                 p-v:0.8781 t-test
#2. Pureba de normalidad:         Cumple.   
#                                 p-v:0.0799 Shapiro-wilk
#3. Prueba de heterocedasticidad: El modelo es homocedástico.
#                                 p-v:NA     Breusch-Pagan
#4. Prueba de correlación serial: No se detecta correlación serial en los errores.
#                                 p-v:0.02455  Durbin-Watson
#5. Prueba RESET:                 El modelo está correctamente especificado.
#                                 p-v:0.5922

# 1. Prueba de media cero:                                                  ####
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(M4_EBITDA.d1$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

# 2. Prueba de normalidad:                                                  ####
# H0 : Los errores son normales
# H1 : Los errores no son normales
shapiro.test(M4_EBITDA.d1$residuals)
library(nortest)
ad.test(M4_EBITDA.d1$residuals)
# p-value > 0.05, no se rechaza HO: en este caso los errores son normales.
qqnorm(M4_EBITDA.d1$residuals)
qqline(M4_EBITDA.d1$residuals, col = "red")

# 3. Prueba de heterocedasticidad:                                          ####
# H0 : Los errores son homocedásticos
# H1 : Los errores no son homocedásticos
library(lmtest)
bptest(M4_EBITDA.d1)
# p-value > 0.05, no se rechaza HO: en este caso los errores cumplen
# con supuestos de homocedásticidad.

# 4. Prueba de correlación serial:                                          ####

# 4.1. Prueba Durbin-Watson
# H0 : Los errores no tienen correlación serial sucesiva
# H1 : Los errores tienen correlación serial sucesiva
dwtest(M4_EBITDA.d1, alternative = "two.sided")
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva.

# 4.2. Prueba Breush-Godfrey
# H0 : Los errores no tienen correlación serial sucesiva hasta orden r
# H1 : Los errores no tienen correlación serial sucesiva hasta orden r
bgtest(M4_EBITDA.d1, order = 3)
bgtest(M4_EBITDA.d1, order = 6)
bgtest(M4_EBITDA.d1, order = 9)
bgtest(M4_EBITDA.d1, order = 12)
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva de orden r.

# 4. Prueba RESET:                                                          ####
# H0 : El modelo está correctamente especificado
# H1 : El modelo no está correctamente especificado
library(lmtest)
library(sandwich)
resettest(M4_EBITDA.d1, power = 2:3, vcov = vcovHAC)
# p-value > 0.05, no se rechaza HO: en este caso el modelo esta correctamente
# especificado.



#   Predict                                                                 ####

datos5 <- data.frame(gas_waha.neg=mean(-gas_waha)+1)
predict(m3, newdata=datos5)
