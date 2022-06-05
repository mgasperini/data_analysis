library(moments)
library(modeest)
library(nortest)
library(ggplot2)
library(dplyr)

# Se cargan los valores
#C.PD = cuerpo - pliegue Derecho
#C.PI = cuerpo - pliegue Izquierdo
#PD.PI = Pliegue derecho - Pliegue izquierdo
#C.PD2 = cuerpo - pliegue Derecho (Recortado al 5%)

C.PD<-c(0.09,-0.09,-0.20,0.25,-0.06,-0.19,-0.07,-0.45,0.07,0.02,-0.05,0.34,-0.25,0.03,-0.02,0.23,-0.31,0.01,0.45,-0.03,0.31,0.21,-0.30,-0.19,-0.30,-0.26,-0.11,-0.26,0.00,0.55,0.15,0.35,0.30,0.15,0.01,0.00,-0.20,-0.21,-0.28,0.13,0.25,0.17,0.29,-0.01,-0.26,-0.15,0.25,-0.08,0.05,0.05,0.43,0.07,-0.13,0.00,-0.14,-0.04,0.33,0.10,-0.17,-0.14,0.12,0.17,0.01,0.14,0.12,0.00,0.09,0.22,0.06,-0.04,-0.03,-0.02,-0.23,-0.01,0.17,0.02,0.03,0.22,0.01,-0.05,0.08,-0.14,0.02,0.35,0.10,0.46,0.00,0.52,0.12,0.02,-0.28,0.51,0.25)
C.PI<-c(0.22,0.02,-0.12,0.47,-0.21,0.13,-0.17,0.21,-0.22,-0.02,0.05,0.18,0.03,-0.14,0.09,0.11,0.07,-0.18,-0.04,0.06,0.14,0.02,0.19,-0.01,0.07,-0.45,0.09,-0.14,-0.32,-0.51,-0.02,0.45,-0.09,-0.27,0.42,0.35,-0.10,0.17,-0.22,-0.07,-0.27,0.49,0.21,-0.09,-0.11,-0.06,-0.12,0.50,-0.19,0.03,0.07,0.36,0.20,0.19,-0.31,-0.09,0.10,0.32,0.25,-0.11,-0.21,0.03,0.14,0.28,0.17,0.04,-0.15,0.14,0.04,0.34,0.08,-0.02,-0.07,-0.24,-0.14,-0.01,0.39,0.24,0.30,0.12,-0.01,-0.03,-0.07,-0.12,-0.03,0.35,0.28,0.21,0.22,0.09,0.02,-0.17,0.30)
PD.PI<-c(0.14,0.11,0.08,0.22,-0.15,0.02,0.28,0.23,-0.09,0.03,0.23,-0.31,0.11,0.06,0.13,-0.16,0.13,-0.05,-0.39,0.17,-0.29,-0.02,0.29,0.26,-0.15,0.12,-0.21,-0.25,-0.02,-0.10,-0.24,-0.62,0.12,0.20,-0.11,0.17,-0.02,0.14,0.01,0.36,-0.04,-0.26,-0.40,-0.05,0.03,0.25,-0.11,-0.02,0.02,-0.07,0.13,0.32,-0.31,0.05,0.14,-0.01,0.15,0.06,-0.07,-0.09,-0.03,0.27,0.03,-0.08,-0.15,0.05,-0.18,0.28,0.12,0.01,-0.05,-0.01,0.00,0.22,0.22,0.27,-0.10,-0.02,0.02,-0.15,0.02,-0.05,0.00,-0.18,0.21,-0.30,-0.03,0.00,0.11,-0.21)

#C.PD recortado al 10%
C.PD.df<-data.frame(C.PD)
C.PD2<-filter(C.PD.df,C.PD.df[,1]<=quantile(C.PD,prob=0.96)&C.PD.df[,1]>=quantile(C.PD,prob=0.05))[,1]




#Pliegue derecho
# Media, Mediana, DE
mediacpd<-mean(C.PD)
mediacpd5<-mean(C.PD,trim=5/100)
mediacpd10<-mean(C.PD,trim=10/100)
medianacpd<-median(C.PD)
desviocpd<-sd(C.PD)
modacpd<-mean(mlv(C.PD,method="mfv"))
nombre_datos_C.PD <- "Diferencia(Cuerpo - Pliegue derecho)" 






#Primera hipótesis sobre C.PD:
# H0 : mu= 0
# Ha : mu <> 0

z<-mediacpd/(desviocpd/sqrt(length(C.PD)))
zcpd <- z
alfa95<-.05
alfa99<-.01
alfa99.9<-.001
z.media.lim99<-qnorm(1-alfa99/2)
z.media.lim95<-qnorm(1-alfa95/2)
z.media.lim99.9<-qnorm(1-alfa99.9/2)
if (abs(z)>abs(z.media.lim99)){
respuesta1cpd <-"Con un 99,9% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else if (abs(z)>abs(z.media.lim99)){
respuesta1cpd <- "Con un 99% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else if (abs(z)>abs(z.media.lim95)){
respuesta1cpd <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else {
respuesta1cpd <-"No hay evidencia suficiente para determinar que la TRT del cuerpo es distinta a la TRT del pliegue derecho (Rechazar H0 con un 95% de confianza)"
}

#Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente


#Segunda hipótesis sobre C.PD
#H0 : mu <= 0
#Ha : mu > 0

z.lim99<-qnorm(1-alfa99)
z.lim95<-qnorm(1-alfa95)
z.lim99.9<-qnorm(1-alfa99.9)

# Si z > z.lim -> Puedo rechazar H0
if (z > z.lim99.9){
respuesta2cpd <-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim99){
respuesta2cpd <-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim95){
respuesta2cpd <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta2cpd <-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es mayor a la TRT del pliegue derecho (Rechazar H0 al 95% de confianza)"
}

#Tercera hipótesis sobre C.PD
#H0 : mu >= 0
#Ha : mu < 0
# Si z < - Z crítico -> Puedo rechazar H0

if (z < -z.lim99.9){
respuesta3cpd <-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim99){
respuesta3cpd <-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim95){
respuesta3cpd <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta3cpd <-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es menor a la TRT del pliegue derecho (Rechazar H0 al 95% de confianza)"
}


#Pliegue derecho al 10%
# Media, Mediana, DE
mediacpd2<-mean(C.PD2)
mediacpd25<-mean(C.PD2,trim=5/100)
mediacpd210<-mean(C.PD2,trim=10/100)
medianacpd2<-median(C.PD2)
desviocpd2<-sd(C.PD2)
modacpd2<-mean(mlv(C.PD2,method="mfv"))
nombre_datos_C.PD2 <- "Diferencia(Cuerpo - Pliegue derecho (10%))" 






#Primera hipótesis sobre C.PD2:
# H0 : mu= 0
# Ha : mu <> 0

z<-mediacpd2/(desviocpd2/sqrt(length(C.PD2)))
zcpd2 <- z
alfa95<-.05
alfa99<-.01
alfa99.9<-.001
z.media.lim99<-qnorm(1-alfa99/2)
z.media.lim95<-qnorm(1-alfa95/2)
z.media.lim99.9<-qnorm(1-alfa99.9/2)
if (abs(z)>abs(z.media.lim99)){
respuesta1cpd2 <-"Con un 99,9% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else if (abs(z)>abs(z.media.lim99)){
respuesta1cpd2 <- "Con un 99% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else if (abs(z)>abs(z.media.lim95)){
respuesta1cpd2 <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT y del pliegue derecho es distinta de 0"
} else {
respuesta1cpd2 <-"No hay evidencia suficiente para determinar que la TRT del cuerpo es distinta a la TRT del pliegue derecho (Rechazar H0 con un 95% de confianza)"
}

#Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente


#Segunda hipótesis sobre C.PD2
#H0 : mu <= 0
#Ha : mu > 0

z.lim99<-qnorm(1-alfa99)
z.lim95<-qnorm(1-alfa95)
z.lim99.9<-qnorm(1-alfa99.9)

# Si z > z.lim -> Puedo rechazar H0
if (z > z.lim99.9){
respuesta2cpd2 <-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim99){
respuesta2cpd2 <-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim95){
respuesta2cpd2 <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es mayor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta2cpd2 <-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es mayor a la TRT del pliegue derecho (Rechazar H0 al 95% de confianza)"
}

#Tercera hipótesis sobre C.PD2
#H0 : mu >= 0
#Ha : mu < 0
# Si z < - Z crítico -> Puedo rechazar H0

if (z < -z.lim99.9){
respuesta3cpd2 <-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim99){
respuesta3cpd2 <-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim95){
respuesta3cpd2 <-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue derecho es menor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta3cpd2 <-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es menor a la TRT del pliegue derecho (Rechazar H0 al 95% de confianza)"
}



# Pliegue izquierdo


# Media, Mediana, DE
mediacpi<-mean(C.PI)
medianacpi<-median(C.PI)
mediacpi5<-mean(C.PI,trim=5/100)
mediacpi10<-mean(C.PI,trim=10/100)
desviocpi<-sd(C.PI)
modacpi<-mean(mlv(C.PI,method="mfv"))
nombre_datos_C.PI <- "Diferencia(Cuerpo - Pliegue izquierdo)" 


#Primera hipótesis sobre C.PI:
# H0 : mu= 0
# Ha : mu <> 0

z<-mediacpi/(desviocpi/sqrt(length(C.PI)))
zcpi<-z


if (abs(z)>abs(z.media.lim99)){
respuesta1cpi<-"Con un 99,9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y el pliegue izquierdo es distinta de 0"
} else if (abs(z)>abs(z.media.lim99)){
respuesta1cpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y el pliegue izquierdo es distinta de 0"
} else if (abs(z)>abs(z.media.lim95)){
respuesta1cpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y el pliegue izquierdo es distinta de 0"
} else {
respuesta1cpi<-"No hay evidencia suficiente para determinar que la diferencia que la TRT del cuerpo es distinta a la TRT del pliegue izquierdo (Rechazar H0 con un 95% de confianza)"
}


#Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente


#Segunda hipótesis sobre C.PI
#H0 : mu <= 0
#Ha : mu > 0


# Si z > z.lim -> Puedo rechazar H0
if (z > z.lim99.9){
respuesta2cpi<-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim99){
respuesta2cpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es mayor que 0 (T Cuerpo > T pliegue)"
} else if (z > z.lim95){
respuesta2cpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es mayor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta2cpi<-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es mayor a la TRT del pliegue izquierdo (Rechazar H0 al 95% de confianza)"
}

#Tercera hipótesis sobre C.PI
#H0 : mu >= 0
#Ha : mu < 0

# Si z < - Z crítico -> Puedo rechazar H0

if (z < -z.lim99.9){
respuesta3cpi<-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim99){
respuesta3cpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es menor que 0 (T Cuerpo > T pliegue)"
} else if (z < -z.lim95){
respuesta3cpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del cuerpo y del pliegue izquierdo es menor que 0 (T Cuerpo > T pliegue)"
} else {
respuesta3cpi<-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del cuerpo es menor a la TRT del pliegue izquierdo (Rechazar H0 al 95% de confianza)"
}

#Diferencia entre pliegues
# Media, Mediana, DE
mediapdpi<-mean(PD.PI)
mediapdpi5<-mean(PD.PI,trim=5/100)
mediapdpi10<-mean(PD.PI,trim=10/100)
medianapdpi<-median(PD.PI)
desviopdpi<-sd(PD.PI)
modapdpi<-mean(mlv(PD.PI,method="mfv"))
nombre_datos_PD.PI <- "Diferencia(Pliegue derecho - Pliegue izquierdo)" 


#Primera hipótesis sobre PD.PI:
# H0 : mu= 0
# Ha : mu <> 0
#Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente

z<-mediapdpi/(desviopdpi/sqrt(length(PD.PI)))
zpdpi<-z

if (abs(z)>abs(z.media.lim99)){
respuesta1pdpi<-"Con un 99,9% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y el pliegue izquierdo es distinta de 0"
} else if (abs(z)>abs(z.media.lim99)){
respuesta1pdpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y el pliegue izquierdo es distinta de 0"
} else if (abs(z)>abs(z.media.lim95)){
respuesta1pdpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y el pliegue izquierdo es distinta de 0"
} else {
respuesta1pdpi<-"No hay evidencia suficiente para determinar que la diferencia que la TRT del pliegue derecho es distinta a la TRT del pliegue izquierdo (Rechazar H0 con un 95% de confianza)"
}

#Segunda hipótesis sobre PD.PI
#H0 : mu <= 0
#Ha : mu > 0

# Si z > z.lim -> Puedo rechazar H0
if (z > z.lim99.9){
respuesta2pdpi<-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es mayor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else if (z > z.lim99){
respuesta2pdpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es mayor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else if (z > z.lim95){
respuesta2pdpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es mayor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else {
respuesta2pdpi<-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del pliegue derecho es mayor a la TRT del pliegue izquierdo (Rechazar H0 al 95% de confianza)"
}

#Tercera hipótesis sobre PD.PI
#H0 : mu >= 0
#Ha : mu < 0

# Si z < - Z crítico -> Puedo rechazar H0

if (z < -z.lim99.9){
respuesta3pdpi<-"Con un 99.9% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es menor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else if (z < -z.lim99){
respuesta3pdpi<-"Con un 99% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es menor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else if (z < -z.lim95){
respuesta3pdpi<-"Con un 95% de confianza, se puede decir que la diferencia entre la TRT del pliegue derecho y del pliegue izquierdo es menor que 0 (T pliegue derecho > T pliegue izquierdo)"
} else {
respuesta3pdpi<-"No hay evidencia suficiente para determinar que la diferencia entre la TRT del pliegue derecho es menor a la TRT del pliegue izquierdo (Rechazar H0 al 95% de confianza)"
}



#Cálculo de Beta

ncpd<-length(C.PD)
ncpd2<-length(C.PD2)
ncpi<-length(C.PI)
npdpi<-length(PD.PI)

# alfa = 0.05 ; C-Pliegue derecho

iniciomediacpd95<- qnorm(alfa95/2,mean=0,sd=desviocpd/sqrt(ncpd))
finmediacpd95<- qnorm(1-alfa95/2,mean=0,sd=desviocpd/sqrt(ncpd))
rango.beta.distinto.cpd<-seq(iniciomediacpd95-0.01,finmediacpd95+0.01,by=0.005)
beta.distinto.cpd95<- pnorm(iniciomediacpd95,mean=rango.beta.distinto.cpd,sd=desviocpd/sqrt(ncpd))+ 1-pnorm(finmediacpd95,mean=rango.beta.distinto.cpd,sd=desviocpd/sqrt(ncpd))
beta.muestra.media.cpd<-pnorm(iniciomediacpd95,mean=mediacpd,sd=desviocpd/sqrt(ncpd))+ 1-pnorm(finmediacpd95,mean=mediacpd,sd=desviocpd/sqrt(ncpd))


iniciocpd95<- qnorm(alfa95,mean=0,sd=desviocpd/sqrt(ncpd))
fincpd95<- qnorm(1-alfa95,mean=0,sd=desviocpd/sqrt(ncpd))

rango.beta.menor.cpd<-seq(iniciocpd95-0.01,mediacpd+0.01,by=0.005)
rango.beta.mayor.cpd<-seq(mediacpd-0.01,fincpd95+0.01,by=0.005)

beta.menor.cpd95<- 1-pnorm(fincpd95,mean=rango.beta.menor.cpd,sd=desviocpd/sqrt(ncpd))
beta.menor.muestra.cpd95<- 1-pnorm(fincpd95,mean=mediacpd,sd=desviocpd/sqrt(ncpd))

beta.mayor.cpd95<- pnorm(iniciocpd95,mean=rango.beta.mayor.cpd,sd=desviocpd/sqrt(ncpd))
beta.mayor.muestra.cpd95<- pnorm(iniciocpd95,mean=mediacpd,sd=desviocpd/sqrt(ncpd))


# alfa = 0.05 ; C-Pliegue derecho

iniciomediacpd295<- qnorm(alfa95/2,mean=0,sd=desviocpd2/sqrt(ncpd2))
finmediacpd295<- qnorm(1-alfa95/2,mean=0,sd=desviocpd2/sqrt(ncpd2))
rango.beta.distinto.cpd2<-seq(iniciomediacpd295-0.01,finmediacpd295+0.01,by=0.005)
beta.distinto.cpd295<- pnorm(iniciomediacpd295,mean=rango.beta.distinto.cpd2,sd=desviocpd2/sqrt(ncpd2))+ 1-pnorm(finmediacpd295,mean=rango.beta.distinto.cpd2,sd=desviocpd2/sqrt(ncpd2))
beta.muestra.media.cpd2<-pnorm(iniciomediacpd295,mean=mediacpd2,sd=desviocpd2/sqrt(ncpd2))+ 1-pnorm(finmediacpd295,mean=mediacpd2,sd=desviocpd2/sqrt(ncpd2))


iniciocpd295<- qnorm(alfa95,mean=0,sd=desviocpd2/sqrt(ncpd2))
fincpd295<- qnorm(1-alfa95,mean=0,sd=desviocpd2/sqrt(ncpd2))

rango.beta.menor.cpd2<-seq(iniciocpd295-0.01,mediacpd2+0.01,by=0.005)
rango.beta.mayor.cpd2<-seq(mediacpd2-0.01,fincpd295+0.01,by=0.005)

beta.menor.cpd295<- 1-pnorm(fincpd295,mean=rango.beta.menor.cpd2,sd=desviocpd2/sqrt(ncpd2))
beta.menor.muestra.cpd295<- 1-pnorm(fincpd295,mean=mediacpd2,sd=desviocpd2/sqrt(ncpd2))

beta.mayor.cpd295<- pnorm(iniciocpd295,mean=rango.beta.mayor.cpd2,sd=desviocpd2/sqrt(ncpd2))
beta.mayor.muestra.cpd295<- pnorm(iniciocpd295,mean=mediacpd2,sd=desviocpd2/sqrt(ncpd2))


# alfa = 0.05 ; C-Pliegue Izquierdo


iniciomediacpi95<- qnorm(alfa95/2,mean=0,sd=desviocpi/sqrt(ncpi))
finmediacpi95<- qnorm(1-alfa95/2,mean=0,sd=desviocpi/sqrt(ncpi))
rango.beta.distinto.cpi<-seq(iniciomediacpi95-0.01,finmediacpi95+0.01,by=0.005)
beta.distinto.cpi95<- pnorm(iniciomediacpi95,mean=rango.beta.distinto.cpi,sd=desviocpi/sqrt(ncpi))+ 1-pnorm(finmediacpi95,mean=rango.beta.distinto.cpi,sd=desviocpi/sqrt(ncpi))


beta.muestra.cpi<-pnorm(iniciomediacpi95,mean=mediacpi,sd=desviocpi/sqrt(ncpi))+ 1-pnorm(finmediacpi95,mean=mediacpi,sd=desviocpi/sqrt(ncpi))

iniciocpi95<-qnorm(alfa95,mean=0,sd=desviocpi/sqrt(ncpi))
fincpi95<-qnorm(1-alfa95,mean=0,sd=desviocpi/sqrt(ncpi))

rango.beta.menor.cpi<-seq(iniciocpi95-0.01,fincpi95+0.01,by=0.005)

rango.beta.mayor.cpi<-seq(iniciocpi95-0.01,fincpi95+0.01,by=0.005)

beta.menor.cpi95<- 1-pnorm(fincpi95,mean=rango.beta.menor.cpi,sd=desviocpi/sqrt(ncpi))
beta.menor.muestra.cpi95<- 1-pnorm(fincpi95,mean=mediacpi,sd=desviocpi/sqrt(ncpi))

beta.mayor.cpi95<- pnorm(iniciocpi95,mean=rango.beta.mayor.cpi,sd=desviocpi/sqrt(ncpi))
beta.mayor.muestra.cpi95<- pnorm(iniciocpi95,mean=mediacpi,sd=desviocpi/sqrt(ncpi))



# alfa = 0.05 ; Pliegue derecho-Pliegue Izquierdo


iniciomediapdpi95<- qnorm(alfa95/2,mean=0,sd=desviopdpi/sqrt(npdpi))
finmediapdpi95<- qnorm(1-alfa95/2,mean=0,sd=desviopdpi/sqrt(npdpi))
rango.beta.distinto.pdpi<-seq(iniciomediapdpi95-0.01,finmediapdpi95+0.01,by=0.005)
beta.distinto.pdpi95<- pnorm(iniciomediapdpi95,mean=rango.beta.distinto.pdpi,sd=desviopdpi/sqrt(npdpi))+ 1-pnorm(finmediapdpi95,mean=rango.beta.distinto.pdpi,sd=desviopdpi/sqrt(npdpi))


beta.muestra.pdpi<-pnorm(iniciomediapdpi95,mean=mediapdpi,sd=desviopdpi/sqrt(npdpi))+ 1-pnorm(finmediapdpi95,mean=mediapdpi,sd=desviopdpi/sqrt(npdpi))

iniciopdpi95<-qnorm(alfa95,mean=0,sd=desviopdpi/sqrt(npdpi))
finpdpi95<-qnorm(1-alfa95,mean=0,sd=desviopdpi/sqrt(npdpi))
rango.beta.menor.pdpi<-seq(iniciopdpi95-0.01,mediapdpi+0.01,by=0.005)
rango.beta.mayor.pdpi<-seq(mediapdpi-0.01,finpdpi95+0.01,by=0.005)
beta.menor.pdpi95<- 1-pnorm(finpdpi95,mean=rango.beta.menor.pdpi,sd=desviopdpi/sqrt(npdpi))
beta.menor.muestra.pdpi95<- 1-pnorm(finpdpi95,mean=mediapdpi,sd=desviopdpi/sqrt(npdpi))

beta.mayor.pdpi95<- pnorm(iniciopdpi95,mean=rango.beta.mayor.pdpi,sd=desviopdpi/sqrt(npdpi))
beta.mayor.muestra.pdpi95<- pnorm(iniciopdpi95,mean=mediapdpi,sd=desviopdpi/sqrt(npdpi))


# 		Intervalos de tolerancia


# C-PD
mediacpd
desviocpd
ncpd
# IC = x +- (valor crítico de tolerancia) * s

#Intervalo unilateral
# Confianza 95%
c95p90<-1.542
c95p95<-1.944
c95p99<-2.706
# Confianza 99%
c99p90<-1.661
c99p95<-2.082
c99p99<-2.883

#Al resultado de las matrices tengo que agregar la media
matriz.tol.95<- matrix(c(c95p90,c95p95,c95p99),nrow=3)
IC95.cpd<-matriz.tol.95*desviocpd
dimnames(IC95.cpd)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC95.cpd
matriz.tol.99<- matrix(c(c99p90,c99p95,c99p99),nrow=3)
IC99.cpd<-matriz.tol.99*desviocpd
dimnames(IC99.cpd)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC99.cpd

# C-PD2
mediacpd2
desviocpd2
ncpd2
# IC = x +- (valor crítico de tolerancia) * s

#Intervalo unilateral
# Confianza 95%
c95p90<-1.542
c95p95<-1.944
c95p99<-2.706
# Confianza 99%
c99p90<-1.661
c99p95<-2.082
c99p99<-2.883

#Intervalo unilateral
# Confianza 95%
c95p9080<-1.559
c95p9580<-1.965
c95p9980<-2.733
# Confianza 99%
c99p9080<-1.688
c99p9580<-2.114
c99p9980<-2.924

#Al resultado de las matrices tengo que agregar la media
matriz.tol.95<- matrix(c(c95p9080,c95p9580,c95p9980),nrow=3)
IC95.cpd2<-matriz.tol.95*desviocpd2
dimnames(IC95.cpd2)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC95.cpd2
matriz.tol.99<- matrix(c(c99p9080,c99p9580,c99p9980),nrow=3)
IC99.cpd2<-matriz.tol.99*desviocpd2
dimnames(IC99.cpd2)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC99.cpd2

matriz.tol.95<- matrix(c(c95p90,c95p95,c95p99),nrow=3)
IC95.cpi<-matriz.tol.95*desviocpi
dimnames(IC95.cpi)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC95.cpi
matriz.tol.99<- matrix(c(c99p90,c99p95,c99p99),nrow=3)
IC99.cpi<-matriz.tol.99*desviocpi
dimnames(IC99.cpi)<-list(c("90% de la población", "95% de la población","99% de la población"),NULL)
IC99.cpi

#------------------------------------------------------------------------------------------

#		Diferencia Cuerpo - Pliegue derecho
summary(C.PD)

mediacpd5      #media recortada al 10%
mediacpd10     #media recortada al 20%
modacpd
desviocpd

#		Verificar normalidad
# Si p-value > 0.05, los datos son normales
shapiro.test(C.PD)
lillie.test(C.PD)

#	Calculo asimetría y curtosis
skewness(C.PD)
kurtosis (C.PD)

#		Primera hipótesis sobre C.PD:
# 	H0 : mu= 0
# 	Ha : mu <> 0
#	Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente
zcpd
z.media.lim95
z.media.lim99
z.media.lim99.9
respuesta1cpd

#		Segunda hipótesis sobre C.PD
#	H0 : mu <= 0
#	Ha : mu > 0
#	Si z > z.lim -> Puedo rechazar H0
zcpd
z.lim95
z.lim99
z.lim99.9
respuesta2cpd

#		Tercera hipótesis sobre C.PD
#	H0 : mu >= 0
#	Ha : mu < 0
#	Si z < - Z crítico -> Puedo rechazar H0
zcpd
z.lim95
z.lim99
z.lim99.9
respuesta3cpd

#------------------------------------------------------------------------------------------

#		Diferencia Cuerpo - Pliegue derecho al 10%
summary(C.PD2)

mediacpd25      #media recortada al 10%
mediacpd210     #media recortada al 20%
modacpd2
desviocpd2

#		Verificar normalidad
# Si p-value > 0.05, los datos son normales
shapiro.test(C.PD2)
lillie.test(C.PD2)

#	Calculo asimetría y curtosis
skewness(C.PD2)
kurtosis (C.PD2)

#		Primera hipótesis sobre C.PD2:
# 	H0 : mu= 0
# 	Ha : mu <> 0
#	Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente
zcpd2
z.media.lim95
z.media.lim99
z.media.lim99.9
respuesta1cpd2

#		Segunda hipótesis sobre C.PD2
#	H0 : mu <= 0
#	Ha : mu > 0
#	Si z > z.lim -> Puedo rechazar H0
zcpd2
z.lim95
z.lim99
z.lim99.9
respuesta2cpd2

#		Tercera hipótesis sobre C.PD2
#	H0 : mu >= 0
#	Ha : mu < 0
#	Si z < - Z crítico -> Puedo rechazar H0
zcpd2
z.lim95
z.lim99
z.lim99.9
respuesta3cpd2

#------------------------------------------------------------------------------------------
#		Diferencia Cuerpo - Pliegue izquierdo
#	Resumen
summary(C.PI)
mediacpi5      #media recortada al 10%
mediacpi10     #media recortada al 20%
modacpi
desviocpi
#		Verificar normalidad
#	Si p-value > 0.05, los datos son normales
shapiro.test(C.PI)
lillie.test(C.PI)
#	Calculo asimetría y curtosis
skewness(C.PI)
kurtosis (C.PI)

#		Primera hipótesis sobre C.PI:
# 	H0 : mu= 0
# 	Ha : mu <> 0
#	Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente
zcpi
z.media.lim95
z.media.lim99
z.media.lim99.9
respuesta1cpi
#		Segunda hipótesis sobre C.PI
#	H0 : mu <= 0
#	Ha : mu > 0
#	Si z > z.lim -> Puedo rechazar H0
zcpi
z.lim95
z.lim99
z.lim99.9
respuesta2cpi

#		Tercera hipótesis sobre C.PI
#	H0 : mu >= 0
#	Ha : mu < 0
#	Si z < - Z crítico -> Puedo rechazar H0
zcpi
z.lim95
z.lim99
z.lim99.9
respuesta3cpi

#------------------------------------------------------------------------------------------
#		Diferencia Pliegue derecho - Pliegue izquierdo
#	Resumen
summary(PD.PI)
mediapdpi5      #media recortada al 10%
mediapdpi10     #media recortada al 20%
modapdpi
desviopdpi


#		Verificar normalidad
#	Si p-value > 0.05, los datos son normales
shapiro.test(PD.PI)
lillie.test(PD.PI)

#	Calculo asimetría y curtosis
skewness(PD.PI)
kurtosis (PD.PI)

#		Primera hipótesis sobre PD.PI:
# 	H0 : mu= 0
# 	Ha : mu <> 0
#	Si |z| > |z.media.lim| -> puedo rechazar H0, sino no tengo evidencia suficiente
zpdpi
z.media.lim95
z.media.lim99
z.media.lim99.9
respuesta1pdpi
#		Segunda hipótesis sobre PD.PI
#	H0 : mu <= 0
#	Ha : mu > 0
#	Si z > z.lim -> Puedo rechazar H0
zpdpi
z.lim95
z.lim99
z.lim99.9
respuesta2pdpi

#		Tercera hipótesis sobre PD.PI
#	H0 : mu >= 0
#	Ha : mu < 0
#	Si z < - Z crítico -> Puedo rechazar H0
zpdpi
z.lim95
z.lim99
z.lim99.9
respuesta3pdpi

#--------------------------------------------------------------------------------------------------------------
#Tablas de resultados

#	Descripción de datos

tabla.resultados<-matrix(ncol=4,nrow=10,byrow=F)
rownames(tabla.resultados)<- c("Media aritmética","Media recortada (10%)","Media recortada (20%)","Mediana","Desvío Estándar","Moda","Mínimo","Máximo","Primer Cuartil","Tercer Cuartil")
colnames(tabla.resultados)<-c("Dif. C-PD","Dif. C-PD2","Dif. C-PI","Dif. PD-PI")
tabla.resultados[,1]<-round(c(mediacpd,mediacpd5,mediacpd10,medianacpd,desviocpd,modacpd,summary(C.PD)[1],summary(C.PD)[6],summary(C.PD)[2],summary(C.PD)[5]),3)
tabla.resultados[,2]<-round(c(mediacpd2,mediacpd25,mediacpd210,medianacpd2,desviocpd2,modacpd2,summary(C.PD2)[1],summary(C.PD2)[6],summary(C.PD2)[2],summary(C.PD2)[5]),3)
tabla.resultados[,3]<-round(c(mediacpi,mediacpi5,mediacpi10,medianacpi,desviocpi,modacpi,summary(C.PI)[1],summary(C.PI)[6],summary(C.PI)[2],summary(C.PI)[5]),3)
tabla.resultados[,4]<-round(c(mediapdpi,mediapdpi5,mediapdpi10,medianapdpi,desviopdpi,modapdpi,summary(PD.PI)[1],summary(PD.PI)[6],summary(PD.PI)[2],summary(PD.PI)[5]),3)


#	Verificación de normalidad

tabla.normalidad<-matrix(ncol=4,nrow=4,byrow=T)
colnames(tabla.normalidad)<-c("Dif. C-PD","Dif. C-PD2","Dif. C-PI","Dif. PD-PI")
rownames(tabla.normalidad)<-c("P-value (Shapiro-Wilks)","P-value (Lilliefors)","Asimetría","Curtosis")
tabla.normalidad[,1]<-round(c(shapiro.test(C.PD)$p.value,lillie.test(C.PD)$p.value,skewness(C.PD),kurtosis (C.PD)),3)
tabla.normalidad[,2]<-round(c(shapiro.test(C.PD2)$p.value,lillie.test(C.PD2)$p.value,skewness(C.PD2),kurtosis (C.PD2)),3)
tabla.normalidad[,3]<-round(c(shapiro.test(C.PI)$p.value,lillie.test(C.PI)$p.value,skewness(C.PI),kurtosis (C.PI)),3)
tabla.normalidad[,4]<-round(c(shapiro.test(PD.PI)$p.value,lillie.test(PD.PI)$p.value,skewness(PD.PI),kurtosis (PD.PI)),3)


#	Hipótesis
# Ho: mu= mu0
# Resultado: 0 - No rechazo Ho; 1 -Rechazo Ho
tabla.hipotesis.distinto<-matrix(ncol=4,nrow=6,byrow=T)
colnames(tabla.hipotesis.distinto)<-c("Dif. C-PD","Dif. C-PD2","Dif. C-PI","Dif. PD-PI")
rownames(tabla.hipotesis.distinto)<-c("z","-z alfa/2","z alfa/2","Error Tipo II - Beta (xmedia)","Potencia de la prueba - 1-Beta(xmedia)","Resultado")
tabla.hipotesis.distinto[,1]<-round(c(zcpd,-z.media.lim95,z.media.lim95,beta.muestra.media.cpd,1-beta.muestra.media.cpd,0),3)
tabla.hipotesis.distinto[,2]<-round(c(zcpd2,-z.media.lim95,z.media.lim95,beta.muestra.media.cpd2,1-beta.muestra.media.cpd2,0),3)
tabla.hipotesis.distinto[,3]<-round(c(zcpi,-z.media.lim95,z.media.lim95,beta.muestra.cpi,1-beta.muestra.cpi,1),3)
tabla.hipotesis.distinto[,4]<-round(c(zpdpi,-z.media.lim95,z.media.lim95,beta.muestra.pdpi,1-beta.muestra.pdpi,0),3)


#Ho: mu <= mu0
# Resultado: 0 - No rechazo Ho; 1 -Rechazo Ho
tabla.hipotesis.menor<-matrix(ncol=4,nrow=5,byrow=T)
colnames(tabla.hipotesis.menor)<-c("Dif. C-PD","Dif. C-PD2","Dif. C-PI","Dif. PD-PI")
rownames(tabla.hipotesis.menor)<-c("z","z alfa","Error Tipo II - Beta (xmedia)","Potencia de la prueba - 1-Beta(xmedia)","Resultado")
tabla.hipotesis.menor[,1]<-round(c(zcpd,z.lim95,beta.menor.muestra.cpd95,1-beta.menor.muestra.cpd95,1),3)
tabla.hipotesis.menor[,2]<-round(c(zcpd2,z.lim95,beta.menor.muestra.cpd295,1-beta.menor.muestra.cpd295,1),3)
tabla.hipotesis.menor[,3]<-round(c(zcpi,z.lim95,beta.menor.muestra.cpi95,1-beta.menor.muestra.cpi95,1),3)
tabla.hipotesis.menor[,4]<-round(c(zpdpi,z.lim95,beta.menor.muestra.pdpi95,1-beta.menor.muestra.pdpi95,0),3)


#Ho : mu>= mu0
# Resultado: 0 - No rechazo Ho; 1 -Rechazo Ho
tabla.hipotesis.mayor<-matrix(ncol=4,nrow=5,byrow=T)
colnames(tabla.hipotesis.mayor)<-c("Dif. C-PD","Dif. C-PD2","Dif. C-PI","Dif. PD-PI")
rownames(tabla.hipotesis.mayor)<-c("z","-z alfa","Error Tipo II - Beta (xmedia)","Potencia de la prueba - 1-Beta(xmedia)","Resultado")
tabla.hipotesis.mayor[,1]<-round(c(zcpd,-z.lim95,beta.mayor.muestra.cpd95,1-beta.mayor.muestra.cpd95,0),3)
tabla.hipotesis.mayor[,2]<-round(c(zcpd2,-z.lim95,beta.mayor.muestra.cpd295,1-beta.mayor.muestra.cpd295,0),3)
tabla.hipotesis.mayor[,3]<-round(c(zcpi,-z.lim95,beta.mayor.muestra.cpi95,1-beta.mayor.muestra.cpi95,0),3)
tabla.hipotesis.mayor[,4]<-round(c(zpdpi,-z.lim95,beta.mayor.muestra.pdpi95,1-beta.mayor.muestra.pdpi95,0),3)

tabla.hipotesis.distinto
respuesta1cpd
respuesta1cpd2
respuesta1cpi
respuesta1pdpi
tabla.hipotesis.menor
respuesta2cpd
respuesta2cpd2
respuesta2cpi
respuesta2pdpi
tabla.hipotesis.mayor
respuesta3cpd
respuesta3cpd2
respuesta3cpi
respuesta3pdpi

#Tabla Intervalo de tolerancia

#95% de confianza
tIT95<-matrix(nrow=3,ncol=3,byrow=T)
dimnames(tIT95)<-list(c("90% de la población", "95% de la población","99% de la población"),c("Dif. C-PD","Dif. C-PD2","Dif. C-PI"))
tIT95[,1]<-IC95.cpd
tIT95[,2]<-IC95.cpd2
tIT95[,3]<-IC95.cpi

#99% de confianza
tIT99<-matrix(nrow=3,ncol=3,byrow=T)
dimnames(tIT99)<-list(c("90% de la población", "95% de la población","99% de la población"),c("Dif. C-PD","Dif. C-PD2","Dif. C-PI"))
tIT99[,1]<-IC99.cpd
tIT99[,2]<-IC99.cpd2
tIT99[,3]<-IC99.cpi


tIT95
tIT99
