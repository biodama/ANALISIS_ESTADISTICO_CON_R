
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


boxplot(datos$"peso" ~ datos$"sexo",
main="Peso",xlab="Sexo",ylab="Peso (Kg)")


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

gmodels::CrossTable(datos$"estado.civil",datos$"sexo",
prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)

library(crosstable)
crosstable(datos,c(estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)

crosstable(datos,c(peso,estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)

######################
# INFERENCIA
######################

rm(list=ls())
gc()

load("/Users/darwin/Desktop/ANALISIS_ESTADISTICO_CON_R_FI_2026/DATOS/datos.curso1.RData")

mujeres<- datos[datos$"sexo"%in%"Mujer",]
mean(mujeres$"peso")
sd(mujeres$"peso")

teorico_mujeres<- rnorm(n=10000, mean = mean(mujeres$"peso"), sd = sd(mujeres$"peso"))
hist(teorico_mujeres)
hist(mujeres$"peso")


res <- t.test(x=mujeres$"peso",conf.level = 0.95)
class(res)
names(res)

res$"estimate"
res$"conf.int"

mean_ic95 <- paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
                   round(res$"conf.int"[2],2),")",sep="")
mean_ic95


# Comparaciones

hombres <- datos[datos$sexo=="Hombre",]
mujeres <- datos[datos$sexo=="Mujer",]

mean(hombres$"peso")-mean(mujeres$"peso")

res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95)
res
names(res)
res$"p.value"


res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95,var.equal=TRUE)
res$"p.value"


res<-t.test(hombres$"peso",mu = 90,sd=1, conf.level = 0.95)
res

res_dos_colas<-t.test(hombres$"peso",mu = 79.80,sd=1,
                      conf.level = 0.95,alternative = c("two.sided"))
res_dos_colas$"p.value"


res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1,
                 conf.level = 0.95,alternative = c("less"))
res_less$"p.value"


res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1,
                 conf.level = 0.95,alternative = c("greater"))
res_less$"p.value"

# Asunciones

shapiro.test(hombres$"peso")

shapiro.test(mujeres$"peso")

var.test(x=hombres$"peso",y=mujeres$"peso")

# test no paremetricos

res<-wilcox.test(hombres$"peso",mujeres$"peso")
res

res<-wilcox.test(hombres$"peso",mu=79.80)
res

res<-t.test(hombres$"peso",mu=79.80)
res


# Mas de dos  grupos

anova(lm(hombres$"peso"~hombres$"nivel.estudios"))

pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),p.adjust="BH")


shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Alto"])

shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Medio"])

shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Bajo"])

bartlett.test(hombres$"peso" ~ hombres$"nivel.estudios")


# No parametrico

kruskal.test(hombres$"peso" ~ hombres$"nivel.estudios")

# En el caso que se observen alguna diferencia entre medias:
# Contrastes dos a dos (post-hoc) ajuste por comparaciones multiples

pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                p.adj="BH")

pairwise.wilcox.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                     p.adj="BH")


#############################
# COMPARACION PROPORCIONES
#############################

# Comparar con un valor 0.40

table(datos$"fumador",exclude=NULL)

prop.table(table(datos$"fumador",exclude=NULL))

prop.test(table(datos$"fumador",exclude=NULL)[2:1])

prop.test(table(datos$"fumador",exclude=NULL)[2:1],correct = FALSE)


# Otras maneras

prop.test(as.numeric(table(datos$"fumador"))[2] , 
          dim(datos)[1])

prop.test(as.numeric(table(datos$"fumador"))[2] , 
          dim(datos)[1] ,
          0.40,
          alternative="greater")

prop.test(92,200, 0.40 ,alternative="greater")


# Comparar la proporcion de fumadores entre hombres y mujeres

table(datos$sexo,datos$fumador)[,c(2,1)]

prop.table(table(datos$sexo,datos$fumador),1)

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)],correct=FALSE)



# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)

# igual que el anova en variables cuantitativas
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])

prop.trend.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2)],
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1))


############
# TABLAS
############

datos1<-subset(datos,select=c(ID,altura,edad,sexo))
datos2<-subset(datos,select=c(ID,diabetes,estado.civil,fumador))

require(gtsummary)
subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo") %>%
add_overall() %>%
add_p(test = everything() ~ "t.test")


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo") %>%
add_overall() %>%
add_p(test = everything() ~ "t.test",
test.args = all_tests("t.test") ~ list(var.equal = TRUE))


subset(datos1,select=-c(ID)) %>%
tbl_summary(by="sexo",statistic = all_continuous() ~ "{mean}") %>%
add_overall() %>%
add_p(test = everything() ~ "t.test")%>%
add_ci(pattern = "{stat} ({ci})")


datos1 <- subset(datos,select=c(ID,altura,edad,sexo))

require("gtsummary")

tbl0<-subset(datos1,select=-c(ID,sexo)) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean}") %>%
  add_ci(pattern = "{stat} ({ci})")

tbl1<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo",
              statistic = all_continuous() ~ "{mean}")%>%
  add_ci(pattern = "{stat} ({ci})")

tbl2<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "t.test")%>%
  modify_column_hide(all_stat_cols())

tbl3<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "wilcox.test")%>%
  modify_column_hide(all_stat_cols())

tbl_final <- 
  tbl_merge(list(tbl0,tbl1, tbl2, tbl3)) %>%
  modify_spanning_header(everything() ~ NA)

tbl_final

# Exportacion

setwd("/Users/pfernandezn/Desktop/")

library("flextable")
tf <- tempfile(fileext = ".docx")
tf<-("tabla_exportar.docx")
ft1 <- as_flex_table(tbl_final)
save_as_docx(ft1, path = tf)

library(flextable)
tf <- tempfile(fileext = ".png")
tf<-("tabla_resultados.png")
ft1 <- as_flex_table(tbl_final)
save_as_image(ft1, path = tf)









