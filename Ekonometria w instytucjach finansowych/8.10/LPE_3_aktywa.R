#Linia portfeli efektywnych: TRZY aktywa ryzykowne

#1. Uogolnic kod na dowolna liczbę aktywow ryzykownych
#2. wybrac portfele optymalne:
#a) o min ryzyku
#b) o min ryzyku z zadana oczekiwana stopa zwrotu.
#c) o maksymalnej oczekiwanej uzyteczności (potegowej + teoria perspektywy)

#3. Napisac funkcje: macierz semikowariancji z proby
#4. Zastosowac macierz semikowariancji z gamma =5/252

#5. Napisac funkcje zwracajace portfele optymalne: 
#a) o min ryzyku
#b) o min ryzyku z zadana oczekiwana stopa zwrotu.




rm(list = ls())

library(tseries)
library(fBasics)#kurtosis
library(zoo)
library(gplots)
# last:
if(!require('xts')) {
  install.packages('xts')
  library('xts')
}
#years:
if(!require('lubridate')) {
  install.packages('lubridate')
  library('lubridate')
}

options(digits=4)  # wyswietla jedynie 4 liczby po przecinku
options(scipen=10) #options(scipen=0) #powrot do e


#WSTEP, OGOLNY: jak wczytywac dane itp.
# loading daily wig20 data
filename = "https://stooq.pl/q/d/l/?s=wig20&i=d"   # i = d --> dane dzienne
x        = read.csv(filename)
y        = zoo(x$Zamkniecie, as.Date(x$Data))
data0    = y

head(data0)

#dataW         = window(data0,start = "2000-01-03",end="2024-10-11")
#head(dataW)
dataW         = window(data0,start = last(index(data0))-years(14))   # ostatnie 14 lat
head(dataW)

par(mar = c(5, 5, 3, 3)) # Set the margin on all sides to 3
plot(dataW, type = "l", col = "blue",xlab="t", ylab="x(t)", 
     main="WIG20",
     #main=" x(t)",
     font.main = 1)


length(dataW)
dataW[1]
dataW[length(dataW)]

#TO SA PRZYROSTY logarytmow = logarytmiczne stopy zwrotu: (na razie niepotrzebne)
lnx=log(dataW)
stopy=diff(lnx,lag=1,differences=1)*100   #log. stopy zwrotu w p.p.
#daty = as.Date(x$Data)
#daty=daty[-1]
rownames(stopy)=rownames(lnx[-1])
#logreturn        = zoo(stopy, daty)
logreturn        = zoo(stopy)


par(mar = c(5, 5, 3, 3)) # Set the margin on all sides to 3
plot(logreturn, type = "l", col = "brown",xlab="t", ylab="y(t)", 
     #main="WIG20",
     #main=" x(t)",
     #font.main = 1
)

###############################################################################################
#DANE Z PLIKU:
#wczytuje dane:
#dane1csv=read.table("jsw_w.csv",sep = ",", dec = ".", header=TRUE, stringsAsFactors=FALSE) #

#KONIEC WSTEPU
rm(list = ls())
#SCIAGAMY DANE PIERWSZEJ SPOŁKI: JSW
# sciaganie dziennych danych spolki JSW
filename = "https://stooq.pl/q/d/l/?s=jsw&i=d"
x        = read.csv(filename)
y        = zoo(x$Zamkniecie, as.Date(x$Data))
data0    = y

# dodawanie danych wchodzacych w sklad indeksu WIG20 
# symbole gieldowe (tickers): https://stooq.pl/t/?i=532

# JSW -	JSW	
# KGH -	KGHM	
# KRU	- KRUK	
# KTY	- KETY	
# LPP	- LPP	
# MBK	- MBANK	
# OPL	- ORANGEPL	
# PCO	- PEPCO	
# PEO	- PEKAO	
# PGE	- PGE	
# PKN	- PKNORLEN	
# PKO	- PKOBP	
# PZU	- PZU	
#tickers = c("kgh","kru","kty","mbk","opl","peo","pge","pkn","pko","pzu") 
tickers = c("kgh", "kru", "pge", "mbk", "pkn")

for(tick in tickers){
  filename = paste("https://stooq.pl/q/d/l/?s=",tick,"&i=d", sep = "")
  x        = read.csv(filename)
  y        = zoo(x$Zamkniecie, as.Date(x$Data))
  data0    = merge(data0,y)
}            
names(data0) = c("jsw",tickers)
data         = window(data0,start = last(index(data0))-years(12))  # 12 ostatnich lat

#zapisywanie do csv
save(data,file="notowania.Rdata")
write.csv(as.data.frame(data),file="notowania.csv")

merged.data=data
class(merged.data)
dim(merged.data)
x.z=merged.data
which(is.na(merged.data))

n=nrow(merged.data)
k=ncol(merged.data)   # liczba kolumn = wymiar portfela

# wykres, wszystko na jednym rysunku
#https://stackoverflow.com/questions/15131926/plot-multiples-time-series-in-r-with-legend
plot(x.z, plot.type="single", col = 1:k, xlab="czas", ylab="wartosc")
legend("topleft", colnames(x.z), col=1:k, lty=1, cex=.65)
title("notowania")


# proste stopy zwrotu
y.m = 100*(as.matrix(x.z[2:n,]) - as.matrix(x.z[1:(n-1),]))/as.matrix(x.z[1:(n-1),])   # w punktach procentowych


par(mar = c(5, 5, 3, 3)) # Set the margin on all sides
plot(as.ts(y.m), type = "l", col = "blue",xlab="t", ylab="y(t)", 
     main="proste zwroty",
     #main=" x(t)",
     font.main = 1)

plot(as.ts(y.m), plot.type="single", col = 1:k, xlab="czas", ylab="wartosc")
legend("topleft", colnames(x.z), col=1:k, lty=1, cex=.65)
title("zwroty")


# statystyki opisowe
apply(y.m, 2, mean)
apply(y.m, 2, sd)
apply(y.m, 2, skewness)
apply(y.m, 2, kurtosis)   # eksces; żeby była kurtoza trzeba dodać 3

#szereg czasowy:
y.m.ts=as.ts(y.m)
cor(y.m.ts)
cov(y.m.ts)

# na podstawie : Kopczewska K., Kopczewski T., Wojcik P. (2009), Metody ilociowe w R. Aplikacje ekonomiczne i finansowe, CeDeWu Sp.z.o.o., Warszawa 2009, str.383

# linia portfeli efektywnych dla 3 aktywow, krotka sprzedaz niedozwolona
nlos=1000 #liczba losowan 

wymiar=k #wymiar portfela
# losujemy 3 wagi, tak żeby się sumowały do 1
if (wymiar==3){
  w1=runif(nlos, min=0, max=1)
  w2=runif(nlos, min=0, max=1)
  w3=1-w1-w2
  suma.wag=w1+w2+w3
  #wagi=cbind(w1,w2,w3, suma.wag)/suma.wag
  wagi=cbind(w1,w2,w3,suma.wag)
  wagi.ok=subset(wagi, w3>=0) #wybor dobrych wag; 
  wagi.m=as.matrix(wagi.ok[,1:wymiar])
  zwroty=y.m.ts[,1:wymiar]
}

# próbkowe statystyki
zwroty.m=as.matrix((colMeans(zwroty)))   # średnie liczone po kolumnach - estymator wartości oczekiwanej stóp zwrotu poszczególnych portfeli
zwrot.portf=wagi.m%*%zwroty.m   # srednie zwroty portfela
colnames(zwrot.portf)="zw_port"
kow=as.matrix(cov(zwroty))
cov.portf=wagi.m%*%kow%*%t(wagi.m)   # kowariancja rozszerzona
var.portf=as.matrix(diag(cov.portf))   # wybor elementow diagonalnych - wariancje poszczególnych wylosowanych portfeli
colnames(var.portf)="var_port"


#LPE - rysunek
plot(var.portf, zwrot.portf, ylim=c(min(zwrot.portf), max(zwrot.portf)))
title(main="Linia portfeli efektywnych - aktywa ryzykowne")
abline(h=max(zwrot.portf), lty=2)
abline(v=min(var.portf), lty=2, col="red")
legend("bottomright", c("portfel o maksymalnym zwrocie ", "portel o minimalnej mozliwej wariancji"), lty=2, col=c("black", "red"), cex=0.8)

plot(w3[w3>=0])

wynikn=cbind(wagi.m,zwrot.portf,var.portf)

if (wymiar==3){
  colnames(wynikn)=c("w1","w2", "w3", "zwrot", "wariancja")
}

write.csv(as.data.frame(wynikn),file="WYNIK.csv")

########### różne k #######################
nlos=1000
wymiar=k #wymiar portfela
# losujemy 3 wagi, tak żeby się sumowały do 1
if (wymiar!=3){
  wagi = matrix(0, ncol = k, nrow = nlos)
  for (i in 1:k){
    wagi[,i] = runif(nlos, min = 0, max = 1)
  }
  suma.wag = rowSums(wagi)
  for (ii in 1:nlos){
    wagi[ii,] = wagi[ii,]/suma.wag[ii]
  }
  wagi.m=as.matrix(wagi[,1:wymiar])
  zwroty=y.m.ts[,1:wymiar]
}

### próbkowe statystyki ###

# macierz semikowariancji
semicov_gamma = function(mzwrot, gamm){
  ##### W DOMU!!!! #####
}

zwroty.m=as.matrix((colMeans(zwroty)))   # średnie liczone po kolumnach - estymator wartości oczekiwanej stóp zwrotu poszczególnych portfeli
zwrot.portf=wagi.m%*%zwroty.m   # srednie zwroty portfela
colnames(zwrot.portf)="zw_port"
kow=as.matrix(cov(zwroty))
cov.portf=wagi.m%*%kow%*%t(wagi.m)   # kowariancja rozszerzona
var.portf=as.matrix(diag(cov.portf))   # wybor elementow diagonalnych - wariancje poszczególnych wylosowanych portfeli
colnames(var.portf)="var_port"


#LPE - rysunek
plot(var.portf, zwrot.portf, ylim=c(min(zwrot.portf), max(zwrot.portf)))
title(main="Linia portfeli efektywnych - aktywa ryzykowne")
abline(h=max(zwrot.portf), lty=2)
abline(v=min(var.portf), lty=2, col="red")
legend("bottomright", c("portfel o maksymalnym zwrocie ", "portel o minimalnej mozliwej wariancji"), lty=2, col=c("black", "red"), cex=0.8)

plot(w6[w6>=0])

wynikn=cbind(wagi.m,zwrot.portf,var.portf)

if (wymiar==6){
  colnames(wynikn)=c("jsw", tickers, "zwrot", "wariancja")
}

write.csv(as.data.frame(wynikn),file="WYNIK.csv")


########################################
##### szukamy portfeli optymalnych #####
########################################

zb_portfeli = round(as.data.frame(wynikn), 8)
head(zb_portfeli)

#Wybieramy portfel o minimalnym ryzyku:

zb_portfeli[zb_portfeli$wariancja == min(zb_portfeli$wariancja), ]

#o największym oczekiwanym zwrocie:
zb_portfeli[zb_portfeli$zwrot == max(zb_portfeli$zwrot), ]

#o minimalnym ryzyku i oczekiwany zwrot >= 10/252
dolny_zwrot = 10/252
zb_portfeli[(zb_portfeli$zwrot >= dolny_zwrot & zb_portfeli$wariancja == min(zb_portfeli$wariancja)), ]



###################
#######################
####################################
#3 Funkcje:

# portfel o minimalnym ryzyku (minimum variance portfolio), krotka sprzedaz dozwolona; ze wzoru analitycznego
#argumenty: wektor wartoci oczekiwanych, macierz kowariancji
m_risk_p=function (vec_mean, m_cov) 
{
 #...
  return(resultf)
}

# portfel o minimalnym ryzyku (minimum variance portfolio) 
#ze zgory zadana oczekiwana stopa zwrotu (rg)
#krotka sprzedaz dozwolona; ze wzorow analitycznych
#stopa zwrotu=rg
m_risk_p_g=function (vec_mean, m_cov,rg) 
{
  #...
  return(resultf)
}

# portfel o minimalnym ryzyku (minimum variance portfolio) 
#ze zgory zadana oczekiwana stopa zwrotu (rg)
#krotka sprzedaz dozwolona; ze wzorow analitycznych
#stopa zwrotu >= rg

m_risk_p_mrg=function (vec_mean, m_cov,rg) 
{
  #...
  return(resultf)
}




#sr_zwrot=as.matrix((colMeans(zwroty)))
#m_kow=as.matrix(cov(zwroty))

#m_risk_p(sr_zwrot,m_kow)
