###------------------------ Modelo plata ----------------- ####
# Preparación y carga de datos:
rm(list=ls())
library(readxl)
library(ggplot2)
datos_0 <- read_excel("Base de datos_B.xlsx")
attach(datos_0)

# Transformación de datos:
library(dplyr)
datos_1 <- datos_0 %>%
  summarise(
    XAG.d=diff(log(XAG)),
    DXY.d=diff(log(DXY)),
    prod_plata.d=diff(log(prod_plata)),
    ingreso_plata.d=diff(log(ingreso_plata)))





#################################################################### Niveles.
M2_plata.d0 <- lm(ingreso_plata~XAG-1)
summary(M2_plata.d0)

################################################################# diff(log(x)
M2_plata.d1 <- lm(ingreso_plata.d~XAG.d-1,
                data=datos_1)
summary(M2_plata.d1)

################################################################# cointegración
y0_1 <- (ingreso_plata)
x0_1 <- (XAG)
library(urca)
jotest=ca.jo(data.frame(y0_1,x0_1), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

################################################################# Raíz unitaria
a=diff(log(ingreso_plata))
b=diff(log(XAG))

library(tseries)
adf.test(a)
pp.test(a)

library(tseries)
adf.test(b)
pp.test(b)

################################################################# Gráfica
datos_1 %>%
  ggplot(aes(x=ingreso_plata.d,
             y=XAG.d))+
  theme_bw()+
  labs(title="ingreso_plata.d~XAG.d",
       y=expression(paste("diff(log(ingreso plata))")),
       x=expression(paste("diff(log(XAG))")))+
  geom_point(col="blue",
             size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(labels=scales::percent,limits=c(-.6,.6))+
  scale_y_continuous(labels=scales::percent,limits=c(-.6,.6))+
  geom_smooth(method="lm",
              se=F,
              fullrange=T,
              col="red")

###------------------------ Pruebas diagnostico: Plata -------------------- ####
#   Pruebas                       Modelo +     p-values

#1. Prueba de media cero:         Cumple.   
#                                 p-v:0.5927 t-test
#2. Pureba de normalidad:         Cumple.   
#                                 p-v:0.1024  Shapiro-wilk
#3. Prueba de heterocedasticidad: El modelo es homocedástico.
#                                 p-v:NA      Breusch-Pagan
#4. Prueba de correlación serial: No se detecta correlación serial en los errores.
#                                 p-v:0.2704  Durbin-Watson
#5. Prueba RESET:                 El modelo está correctamente especificado.
#                                 p-v:0.457

# 1. Prueba de media cero:                                                  ####
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(M2_plata.d1$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

# 2. Prueba de normalidad:                                                  ####
# H0 : Los errores son normales
# H1 : Los errores no son normales
shapiro.test(M2_plata.d1$residuals)
library(nortest)
ad.test(M2_plata.d1$residuals)
# p-value > 0.05, no se rechaza HO: en este caso los errores son normales.
qqnorm(M2_plata.d1$residuals)
qqline(M2_plata.d1$residuals, col = "red")

# 3. Prueba de heterocedasticidad:                                          ####
# H0 : Los errores son homocedásticos
# H1 : Los errores no son homocedásticos
library(lmtest)
bptest(M2_plata.d1)
# p-value > 0.05, no se rechaza HO: en este caso los errores cumplen
# con supuestos de homocedásticidad.

# 4. Prueba de correlación serial:                                          ####

# 4.1. Prueba Durbin-Watson
# H0 : Los errores no tienen correlación serial sucesiva
# H1 : Los errores tienen correlación serial sucesiva
dwtest(M2_plata.d1, alternative = "two.sided")
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva.

# 4.2. Prueba Breush-Godfrey
# H0 : Los errores no tienen correlación serial sucesiva hasta orden r
# H1 : Los errores no tienen correlación serial sucesiva hasta orden r
bgtest(M2_plata.d1, order = 3)
bgtest(M2_plata.d1, order = 6)
bgtest(M2_plata.d1, order = 9)
bgtest(M2_plata.d1, order = 12)
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva de orden r.

# 4. Prueba RESET:                                                          ####
# H0 : El modelo está correctamente especificado
# H1 : El modelo no está correctamente especificado
library(lmtest)
library(sandwich)
resettest(M2_plata.d1, power = 2:3, vcov = vcovHAC)
# p-value > 0.05, no se rechaza HO: en este caso el modelo esta correctamente
# especificado.
