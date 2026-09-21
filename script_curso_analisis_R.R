
rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_FI_2026/DATOS")

load("datos.curso1.RData")

######################
# Variable cuantitativa
######################

# Estadisticos de tendencia central

mean(datos$"peso")

median(datos$"peso")

exp(mean(log(datos$"peso")))


# Medidas de posicion

quantile(datos$"peso")

quantile(datos$"peso",prob=seq(0,1,1/4)) # esta es la estimacion que viene por defecto

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso",
prob=seq(0,1,1/3)),right=TRUE, include.lowest=TRUE)

table(datos$"peso.gr",exclude=NULL)

table(datos$"peso_cat")

quantile(datos$"peso",prob=seq(0,1,1/3))

quantile(datos$"peso",prob=seq(0,1,1/10))


# Medidas de dispersion

sd(datos$"peso")

var(datos$"peso")

IQR(datos$"peso")

?quantile

min(datos$"peso")

max(datos$"peso")

range(datos$"peso") # minimo y maximo


# Graficos

hist(datos$"peso")

hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

qqnorm(datos$"peso")

boxplot(datos$"peso")


summary(datos[,c(1:4)])

# Tablas

library("crosstable")

res<-crosstable(datos, c(peso,edad))

library("knitr")

kable(res)

##################################
# Variable cuantitativa agrupada
##################################


tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",sd)

tapply(datos$"peso",datos$"sexo",IQR)


library(crosstable)

res<-crosstable(datos, c(peso), by=sexo)
res

res<-crosstable(datos, c(peso), by=sexo,funs=c(mean, quantile))
res

# Graficos

par(mfrow=c(1,2)) # (Numero de filas y numero de columnas del grafico)
hist(datos$"peso"[datos$"sexo"%in%"Hombre"],
main="Peso Hombres",xlab="Peso (Kg)")
hist(datos$"peso"[datos$"sexo"%in%"Mujer"],
main="Peso Mujeres",xlab="Peso (Kg)")


par(mfrow=c(2,1)) # (Numero de filas y numero de columnas del grafico)
hist(datos$"peso"[datos$"sexo"%in%"Hombre"],
main="Peso Hombres",xlab="Peso (Kg)",xlim=c(50,90))
hist(datos$"peso"[datos$"sexo"%in%"Mujer"],
main="Peso Mujeres",xlab="Peso (Kg)",xlim=c(50,90))


hist(datos$"peso",main="Peso",breaks=100)


boxplot(datos$"peso" ~ datos$"sexo",main="Peso",xlab="Sexo",ylab="Peso (Kg)")


#############################
# VARIABLES CUALITATIVAS (character o factor en R; grupos de edad, nivel de estudios...)
#############################

# Frecuencias absolutas (N, numero de registros en cada categoria o valor)

table(datos$"estado.civil",exclude=NULL) # exclude=NULL permite contabilizar los NA values

as.data.frame(table(datos$"estado.civil",exclude=NULL))


# Frecuencias relativas

prop.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100


# Graficos cualitativas

barplot(table(datos$"estado.civil",exclude=NULL)) # frecuencias absolutas (N)

barplot(prop.table(table(datos$"estado.civil"))*100,
ylim=c(0,50)) # frecuencias relativas (%)

pie(table(datos$"estado.civil"))

##################################
# Variables cualitativas agrupadas
##################################

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)


prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100
prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100
prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100


library(crosstable)
crosstable(datos,c(estado.civil),by=sexo)

crosstable(datos,c(peso,estado.civil),by=sexo)

gmodels::CrossTable(datos$"estado.civil",datos$"sexo",prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)

library(crosstable)crosstable(datos,c(estado.civil),by=sexo)%>%as_flextable(keep_id=TRUE)

crosstable(datos,c(peso,estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)

