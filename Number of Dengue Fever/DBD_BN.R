# Persiapan Data untuk Regresi POISSON, NB dan GP
Data <- read.csv2("DBDJateng.CSV",header=TRUE)

head(Data) # untuk melihat nama variabel 
str(Data) # menyimpan data dalam data.frame
summary(Data)

#===========================================
# Menguji distribusi variabel Respon Poisson
#===========================================
ks.test(Data$DBD,"ppois",lambda<-mean(Data$DBD) )

#===========================================
#Uji Multikolinearitas
#===========================================
library(car)
model=lm(DBD~.,data=Data)
vif(model)

#===========================================
# model binomial negatif 
#===========================================
# SOLUSI dengan regresi NB
# install.packages("MASS")
library(MASS)
regnb=glm.nb(DBD~KepadatanPenduduk+KetinggianWilayah+CurahHujan+PresentaseSanitasi,data=Data)
summary(regnb)

#===========================================
#Likelihood ratio test
#===========================================
install.packages("lrtest")
library(lrtest)
regnb1=glm.nb(DBD~NULL,data=Data)
lrtest(regnb1,regnb)

