### Progetto di FP&S: Spotify Tracks Dataset"
## Riccardo Cervero,794126

##CARICAMENTO PACCHETTI##

library(plyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(memisc)
library(e1071)
library(EnvStats)
library(reshape2)
library(car)
library(broom)

###############################################################################

##FUNZIONI##

#ESTRAZIONE DELLA MODA
moda <- function(x) {return(unique(x)[which.max(tabulate(match(x, unique(x))))])}

#CALCOLO DELL'INDICE DI GINI NORMALIZZATO
G_normalizzato <- function(x){
  G=1-sum((table(x)/length(x))^2)
  n=length(unique(x))
  return(G*(n/(n-1)))
}

#RAGGRUPPAMENTO DELLE OSSERVAZIONI PER DECENNI
to_decade <- function(v){return(v-v%%10)}

#CALCOLO DEL COEFFICIENTE DI VARIAZIONE
cv <- function(x){return(sd(x)/abs(mean(x)))}

#CALCOLO DELL'INDICE DI SIMMETRIA
m3 <- function(x){return(skewness(x,na.rm=T))}

#CALCOLO DELLA CURTOSI
m4 <- function(x){return(kurtosis(x,na.rm=T,method="moment",excess=F))}

#ANALISI PRELIMINARE DI UNA QUALSIASI VARIABILE NUMERICA
AnPrel <- function(col) {
  
  #Statistiche preliminari
  s<-summary(col)
  stat <- c()
  for(i in 1:6) {stat<-c(stat,s[[i]])}
  #Moda
  m<-moda(col)
  #Varianza
  v<-var(col)
  #Coefficiente di variazione
  ccv<-cv(col)
  #Asimmetria
  as<-m3(col)
  if_Z <- function(){if(as==0){return("simmetrica")}else if(as>0){return("asimmetrica positiva")}else if(as<0){return("asimmetrica negativa")}}
  #Curtosi
  cu<-m4(col)
  if_C <- function(){if(cu==3){return("normale.")}else if(cu>3){return("leptocurtica.")}else if(cu<3){return("platicurtica.")}}
  
  stat<-c(stat,m,v,ccv,as,cu)
  indici <- c("Minimo","1o Qu.","Mediana","Media","3o Qu.","Massimo","Moda","Varianza","CdV","Asimmetria","Curtosi")
  stat <- data.frame(cbind(indici,stat))
  names(stat) <- c("Statistica","Valore")
  
  #Grafico della distribuzione
  if((typeof(col)=="double")||(col=db$duration_ms)){
    g<-ggplot(db, aes(x=col))+
      geom_histogram(binwidth = 0.01, color= "red", fill="white")+
      geom_vline(aes(xintercept=mean(col)),color="blue", linetype="dashed", size=0.5)+
      geom_vline(aes(xintercept=median(col)),color="green", linetype="dashed", size=0.5)+
      ylab(label="")+
      xlab(label=substring(deparse(substitute(col)), 4))+
      labs(title=paste("Distribuzione di frequenza di",substring(deparse(substitute(col)), 4)),caption=
           "Le linee verticali tratteggiate corrispondono a media (in blue) e mediana (in verde).")
           } else if(typeof(col)=="integer"){
             g<-ggplot(db, aes(x=col))+
               geom_bar(binwidth = 0.01, color= "red", fill="white")+
               geom_vline(aes(xintercept=mean(col)),color="blue",linetype="dashed", size=0.5)+
               geom_vline(aes(xintercept=median(col)),color="green", linetype="dashed", size=0.5)+
               ylab(label="")+
               xlab(label=substring(deparse(substitute(col)), 4))+
               labs(title=paste("Distribuzione di frequenza di",substring(deparse(substitute(col)), 4)),caption=
                    "Le linee verticali tratteggiate corrispondono a media (in blue) e mediana (in verde).")
           }
  
  print(paste("La distribuzione è",if_Z(),"e",if_C()))
  return(list("stat"=stat,
              "distr"=g,
              "qqnorm"=function(){
    qqnorm(col)
    qqline(col)
  }))
}


#INFERENZA DELLA MEDIA DI UNA VAR. QUANTITATIVA
ICmedia <- function(col,l=0.95){
  n <- nrow(db)
  mu <- mean(col)
  sds <- sd(col)
  E <- qnorm(l+(1-l)/2)*sds/sqrt(n)
  return(c(mu-E, mu+E))
}

#TEST DELLA CORRELAZIONE LINEARE DI PEARSON (95%)
rhotest<-function(x,y){
  r<-cor.test(x,y,method="pearson")
  if(r$p.value>0.05){print("H0 accettata")}else{print("H0 rifiutata (corr. significativa)")}
  print(paste0("pvalue: ",r$p.value))
  print(paste0("rho: ",r$estimate))
}

###############################################################################

##DATABASE##

#Link database: https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
#Link documentazione ufficiale: https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/ 

#CARICAMENTO DEL DATASET
db <- read.csv("/Users/riccardocervero/Desktop/DataScience/Materie/Foundation of Prob & Stats/Progetto FPS - Riccardo Cervero/SpotifyDb.csv")

#DIMENSIONI DEL DATASET
dim(db)

#VARIABILI: NOME, TIPOLOGIA E NUMERO DI MODALITÀ
for (c in colnames(db)){
 print(paste0(c," : ",typeof(db[[c]]),", ",length(unique(db[[c]])))) 
}

###############################################################################

##PRE-PROCESSING##

#RIMOZIONE DELLE VARIABILI INUTILI
vi <- c("artists","id","name")
db <- db[ , !(colnames(db) %in% vi), drop=FALSE]

#IDENTIFICAZIONE MISSING VALUE
db[!complete.cases(db),]

#MISSING VALUES IN "KEY"?
any(db$key==-1)

#UNIFORMARE "RELEASE_DATE"
date_format <- c()
for(i in 1:length(release_date)) {
  if(!((class(try(as.Date(release_date[i], format= "%Y-%m-%d")))=="try-error")||(is.na(try(as.Date(release_date[i], format= "%Y-%m-%d")))))) {
  date_format <- c(date_format,"YMD")
} else if(!((class(try(as.Date(release_date[i], format= "%Y-%m")))=="try-error")||(is.na(try(as.Date(release_date[i], format= "%Y-%m")))))) {
  date_format <- c(date_format,"YM")
  } else if(!((class(try(as.Date(release_date[i], format= "%Y")))=="try-error")||(is.na(try(as.Date(release_date[i], format= "%Y")))))) {
  date_format <- c(date_format,"Y")
  }
}
table(date_format)
100*table(date_format)/length(date_format)

###############################################################################

##ANALISI DELLE VARIABILI QUALITATIVE##

#ANALISI DI "Year"

#Tabella di frequenza
print("Frequenze relative:")
round(table(db$year)/length(db$year),4)

#Indice di Gini
print("Indice di Gini normalizzato:")
G_normalizzato(db$year)

#Raggruppamento in decenni
db$decade <- unlist(lapply(db$year,to_decade))
db <- db[ , !(colnames(db) %in% c("release_date","year")), drop=FALSE]

#Tabella di frequenza dei decenni
print("Frequenze relative:")
round(table(db$decade)/length(db$decade),4)

#PieChart dei decenni
y <- c()
for(i in 1:length(unique(db$decade))) {y<-c(y,table(db$decade)[[i]])}
data <- data.frame(Decennio=as.factor(unique(db$decade)),value=y)
data$label <- scales::percent(data$value/sum(data$value))
ggplot(data, aes(x="", y=value, fill=Decennio)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#Test del Chi Quadro di connessione fra decennio e popolarità
table=table(db$decade, db$popularity)
chi=chisq.test(table) 
chi

#Boxplot condizionato
ggplot(db, aes(x=db$decade, y=db$popularity, group=db$decade)) + 
  geom_boxplot()+
  xlab(label = "Decennio")+
  ylab(label = "Popolarità")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 11))

#Test ANOVA per l'uguaglianza delle medie
summary(aov(popularity ~ as.factor(decade), db))

# # # # # # # # # # # # # # # # # # # # # #

#ANALISI DI "Explicit"

#Tabella di frequenza
print("Frequenze relative:")
round(table(db$explicit)/length(db$explicit),4)

#Indice di Gini
print("Indice di Gini normalizzato:")
G_normalizzato(db$explicit)

#PieChart di "explicit"
expl <- mapvalues(db$explicit,c(1,0),c("Sì","No"))
y <- c()
for(i in 1:length(unique(expl))) {y<-c(y,table(expl)[[i]])}
data <- data.frame(Esplicito=as.factor(unique(expl)),value=y)
data$label <- scales::percent(data$value/sum(data$value))
ggplot(data, aes(x="", y=value, fill=Esplicito)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#Stima intervallare sulla proporzione di contenuto esplicito (con 𝛼=5%)
p <- unlist(table(db$explicit)/length(db$explicit))[[2]] #Freq relativa di contenuto esplicito
p0 <- unlist(table(db$explicit)/length(db$explicit))[[1]] #Freq relativa di contenuto non esplicito
n <- nrow(db) #Dimensione del campione
l <- 0.95   #Livello di confidenza
c <- l+(1-l)/2  
SE <- sqrt(p*(1-p)/n) #Deviazione standard di p 
E  <- qnorm(c)*SE
IC <- p+c(-E,E) #intervallo di confidenza per p (esplicito)
IC0 <- p0+c(-E,E) #intervallo di confidenza per p (non esplicito)
print(paste0("IC della proporzione di contenuto eplicito:  (",IC[1],", ",IC[2],")"))
print(paste0("IC della proporzione di contenuto non eplicito:  (",IC0[1],", ",IC0[2],")"))

#Test del Chi Quadro di connessione fra popolarità e presenza di contenuto esplicito
table=table(db$explicit, db$popularity)
chi=chisq.test(table) 
chi

#Boxplot condizionato
ggplot(db, aes(x=mapvalues(db$explicit,c(1,0),c("Sì","No")), y=db$popularity, group=mapvalues(db$explicit,c(1,0),c("Sì","No")))) + 
  geom_boxplot()+
  xlab(label = "Esplicito")+
  ylab(label = "Popolarità")

#Test ANOVA per l'uguaglianza delle medie
summary(aov(popularity ~ as.factor(explicit), db))

# # # # # # # # # # # # # # # # # # # # # #

#ANALISI DI "Mode"

#Tabella di frequenza
print("Frequenze relative:")
round(table(db$mode)/length(db$mode),4)

#Indice di Gini
print("Indice di Gini normalizzato:")
G_normalizzato(db$mode)

#PieChart di "mode"
expl <- mapvalues(db$mode,c(1,0),c("Maggiore","Minore"))
y <- c()
for(i in 1:length(unique(expl))) {y<-c(y,table(expl)[[i]])}
data <- data.frame(Modalità=as.factor(unique(expl)),value=y)
data$label <- scales::percent(data$value/sum(data$value))
ggplot(data, aes(x="", y=value, fill=Modalità)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#Stima intervallare sulla proporzione della modalità (con 𝛼=5%)
p <- unlist(table(db$mode)/length(db$mode))[[2]] #Freq relativa di tonalità "major"
p0 <- unlist(table(db$mode)/length(db$mode))[[1]] #Freq relativa di tonalità "minor"
n <- nrow(db) #Dimensione del campione
l <- 0.95   #Livello di confidenza
c <- l+(1-l)/2  
SE <- sqrt(p*(1-p)/n) #Deviazione standard di p 
E  <- qnorm(c)*SE
IC <- p+c(-E,E) #intervallo di confidenza per p di "major"
IC0 <- p0+c(-E,E) #intervallo di confidenza per p di "minor"
print(paste0("IC della proporzione di tonalità maggiore:  (",IC[1],", ",IC[2],")"))
print(paste0("IC della proporzione di tonalità minore:  (",IC0[1],", ",IC0[2],")"))

#Test del Chi Quadro di connessione fra popolarità e tonalità
table=table(db$mode, db$popularity)
chi=chisq.test(table) 
chi

#Boxplot condizionato
ggplot(db, aes(x=mapvalues(db$mode,c(1,0),c("Maggiore","Minore")), y=db$popularity, group=mapvalues(db$mode,c(1,0),c("Maggiore","Minore")))) +
  geom_boxplot()+
  xlab(label = "Modalità")+
  ylab(label = "Popolarità")

#Test ANOVA per l'uguaglianza delle medie
summary(aov(popularity ~ as.factor(mode), db))

# # # # # # # # # # # # # # # # # # # # # #

#ANALISI DI "Key"

#Tabella di frequenza
print("Frequenze relative:")
round(table(db$key)/length(db$key),4)

#Moda
print("Moda:")
moda(db$key)

#Indice di Gini
print("Indice di Gini normalizzato:")
G_normalizzato(db$key)

#PieChart di "key"
expl <- mapvalues(db$key,c(0,1,2,3,4,5,6,7,8,9,10,11),c("Do","Do♯","Re","Re♯","Mi","Fa","Fa♯","Sol","Sol♯","La","La♯","Si"))
y <- c()
for(i in 1:length(unique(expl))) {y<-c(y,table(expl)[[i]])}
data <- data.frame(Chiave=as.factor(unique(expl)),value=y)
data$label <- scales::percent(data$value/sum(data$value))
ggplot(data, aes(x="", y=value, fill=Chiave)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#Stima intervallare sulla proporzione di Do (con 𝛼=5%)
p <- unlist(table(db$key)/length(db$key))[[1]] #Frequenza relativa di Do
n <- nrow(db) #Dimensione del campione
l <- 0.95   #Livello di confidenza
c <- l+(1-l)/2  
SE <- sqrt(p*(1-p)/n) #Deviazione standard di p 
E  <- qnorm(c)*SE
IC <- p+c(-E,E) #intervallo di confidenza per p di "major"
print(paste0("IC della proporzione di tracce in Do:  (",IC[1],", ",IC[2],")"))

#Do può essere stimata, con confidenza al 95%, come chiave più frequente?
ICp <- function(k){
  p <- unlist(table(db$key)/length(db$key))[[k]]
  n <- nrow(db)
  SE <- sqrt(p*(1-p)/n)
  E  <- qnorm(0.975)*SE
  IC <- p+c(-E,E)
  if(IC[2]>=0.124951684177052){print(paste0(k-1,": TRUE"))}else{print(paste0(k-1,": IC minore rispetto alla classe 'Do'"))}
}
for(k in 2:12){ICp(k)} 
#Calcolati gli IC delle p di ogni chiave, si verifica se almeno un estremo superiore è pari o maggiore all'estremo inferiore dell'IC della p della chiave Do

#Test del Chi Quadro di connessione fra popolarità e chiave
table=table(db$key, db$popularity)
chi=chisq.test(table) 
chi

#Boxplot condizionato
ggplot(db, aes(x=mapvalues(db$key,c(0,1,2,3,4,5,6,7,8,9,10,11),c("Do","Do♯","Re","Re♯","Mi","Fa","Fa♯","Sol","Sol♯","La","La♯","Si")),
               y=db$popularity, group=mapvalues(db$key,c(0,1,2,3,4,5,6,7,8,9,10,11),c("Do","Do♯","Re","Re♯","Mi","Fa","Fa♯","Sol","Sol♯","La","La♯","Si")))) +
  geom_boxplot()+
  xlab(label = "Chiave")+
  ylab(label = "Popolarità")

#Test ANOVA per l'uguaglianza delle medie
summary(aov(popularity ~ as.factor(key), db))

###############################################################################
  
##ANALISI DELLE VARIABILI QUANTITATIVE##

#ANALISI DI "Acousticness"

acoustic <- AnPrel(db$acousticness)

#Statistiche di base
acoustic$stat

#Distribuzione
acoustic$distr

#Normalità
acoustic$qqnorm()

#Stima intervallare dell'acusticità media dal 1921 ad oggi (al 95%)
ICmedia(db$acousticness)

#I dati si riferiscono pertanto a tracce con una probabilità media di essere completamente acustiche - stimata con confidenza al 95% -
#inferiore rispetto alla probabilità media di includere strumenti elettronici, poichè l'intervallo di confidenza di "acousticness" ha estremo superiore inferiore a 0.5.

# # # # # # # # # # # # # # # # # # # # # #
  
#ANALISI DI "Danceability"

dance <- AnPrel(db$danceability)

#Statistiche di base
dance$stat

#Distribuzione
dance$distr

#Normalità
dance$qqnorm()

#Stima intervallare della ballabilità media dal 1921 ad oggi (al 95%)
ICmedia(db$danceability)

# # # # # # # # # # # # # # # # # # # # # #

#ANALISI DI "Duration_ms"

duration <- AnPrel(db$duration_ms)

#Statistiche di base
duration$stat

#Distribuzione
duration$distr

#Normalità
duration$qqnorm()

#Stima intervallare della durata media dal 1921 ad oggi (al 95%)
ICmedia(db$duration_ms)

#La stima - con livello di confidenza al 95% - della durata media dei brani è compresa fra circa 3 minuti e 51 secondi e 3 minuti e 52 secondi.

# # # # # # # # # # # # # # # # # # # # # #
  
#ANALISI DI "Energy"

ener <- AnPrel(db$energy)

#Statistiche di base
ener$stat

#Distribuzione
ener$distr

#Normalità
ener$qqnorm()

#Stima intervallare dell'energia media dal 1921 ad oggi (al 95%)
ICmedia(db$energy)

# # # # # # # # # # # # # # # # # # # # # #

#ANALISI DI "Instrumentalness"

instrumental <- AnPrel(db$instrumentalness)

#Statistiche di base
instrumental$stat

#Distribuzione
instrumental$distr

#Normalità
instrumental$qqnorm()

#Stima intervallare di "strumentalità" media dal 1921 ad oggi (al 95%)
ICmedia(db$instrumentalness)

#La quantità media di contenuto esclusivamente strumentale all'interno delle tracce, con un livello di confidenza del 95%, viene pertanto stimata scarsa,
#al massimo pari a circa il 16.3%.

# # # # # # # # # # # # # # # # # # # # # #
  
#ANALISI DI "Liveness"

live <- AnPrel(db$liveness)

#Statistiche di base
live$stat

#Distribuzione
live$distr

#Normalità
live$qqnorm()

#Stima intervallare della presenza media di pubblico nella registrazione (al 95%)
ICmedia(db$liveness)

#La stima della probabilità media di presenza di un'audience durante la registrazione del brano è, con confidenza al 95%, compresa fra circa il 20.6% e 20.8%.

# # # # # # # # # # # # # # # # # # # # # #
  
#ANALISI DI "Loudness"

loud <- AnPrel(db$loudness)

#Statistiche di base
loud$stat

#Distribuzione
loud$distr

#Normalità
loud$qqnorm()

#Stima intervallare del volume complessivo medio dal 1921 ad oggi (al 95%)
ICmedia(db$loudness)

#Il volume complessivo medio viene stimato, con alfa a 0.05, tra circa i -11.4 decibel e -11.3 decibel

# # # # # # # # # # # # # # # # # # # # # #
  
#ANALISI DI "Speechiness"

speech <- AnPrel(db$speechiness)

#Statistiche di base
speech$stat

#Distribuzione
speech$distr

#Normalità
speech$qqnorm()

#Stima intervallare del volume complessivo medio dal 1921 ad oggi (al 95%)
ICmedia(db$speechiness)

#La probabilità media di rilevare del parlato all'interno delle canzoni è molto bassa, stimata con un livello di confidenza del 95% tra circa il 9.3% e il 9.5%.

# # # # # # # # # # # # # # # # # # # # # #  
  
#ANALISI DI "Tempo"

t<- AnPrel(db$tempo)

#Statistiche di base
t$stat

#Distribuzione
t$distr

#Normalità
t$qqnorm()

#Stima intervallare dei bpm medi dal 1921 ad oggi (al 95%)
ICmedia(db$tempo)

# # # # # # # # # # # # # # # # # # # # # #  

#ANALISI DI "Valence"

val<- AnPrel(db$valence)

#Statistiche di base
val$stat

#Distribuzione
val$distr

#Normalità
val$qqnorm()

#Stima intervallare della positività media trasmessa (al 95%)
ICmedia(db$valence)

#Supponendo indipendenti le decadi, è possibile rintracciare una connessione fra il decennio in cui la traccia è stata pubblicata e il livello emotivo trasmesso?
#Test del Chi quadro di connessione fra positività e decennio
table=table(db$decade, db$valence)
chi=chisq.test(table) 
chi

#Boxplot condizionato
ggplot(db, aes(x=decade, y=valence, group=decade)) +
  geom_boxplot()+
  xlab(label = "Decennio")+
  ylab(label = "Positività")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 11))

#Test ANOVA per l'uguaglianza delle medie
summary(aov(valence ~ as.factor(decade), db))

#È poi possibile che la positività trasmessa possa dipendere statisticamente dalla tonalità?
#Test del Chi quadro di connessione fra positività e tonalità
chisq.test(table(db$mode, db$valence)) 

#Test ANOVA per l'uguaglianza delle medie
summary(aov(valence ~ as.factor(mode), db))

#Si poi valuta una connessione fra chiave globale della traccia e livello di positività:
#Test del Chi quadro di connessione fra positività e chiave
chisq.test(table(db$key, db$valence)) 

#Test ANOVA per l'uguaglianza delle medie
summary(aov(valence ~ as.factor(key), db))

# # # # # # # # # # # # # # # # # # # # # #   

#ANALISI DI "Popularity"

pop<- AnPrel(db$popularity)

#Statistiche di base
pop$stat

#Distribuzione
pop$distr

#Normalità
pop$qqnorm()

###############################################################################
  
##CORRELAZIONI LINEARI##

#Sono stimati i coefficienti di correlazione di Pearson per tutte le possibili coppie di variabili quantitative ed effettuati i corrispettivi test
#con livello di confidenza fissato al 95% per valutare la significatività statistica delle correlazioni più interessanti.

#MATRICE DI CORRELAZIONE
db_num <- db[ , !(colnames(db) %in% c("key","mode","explicit","decade")), drop=FALSE]
cormat <- round(cor(db_num),2)
melted_cormat <- melt(cormat)
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#HEATMAP
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 9, hjust = 1))+
  coord_fixed()+ 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5))
ggheatmap

#TEST PER IL COEFFICIENTE DI CORRELAZIONE DI PEARSON
  
rhotest(db$danceability,db$valence)

rhotest(db$popularity,db$acousticness)

rhotest(db$popularity,db$energy)

rhotest(db$popularity,db$loudness)

rhotest(db$energy,db$loudness)

rhotest(db$energy,db$acousticness)

rhotest(db$loudness,db$acousticness)

rhotest(db$loudness,db$instrumentalness)

rhotest(db$danceability,db$tempo)

rhotest(db$liveness,db$valence)

###############################################################################

##REGRESSIONE SEMPLICE##

#MAPPATURA DELLE VARIABILI FRA 0 E 1 (per rendere confrontabili le stime dei coefficienti di regressione)
ScaleMinMax <- function(x){return((x-min(x))/(max(x)-min(x)))}
dbs <- db
for (c in c("acousticness","danceability","duration_ms","energy","instrumentalness","liveness","loudness","popularity","speechiness","tempo","valence" )){
 dbs[c] <- ScaleMinMax(dbs[c]) 
}

#CONVERSIONE DELLE CATEGORICHE IN "FACTOR"
dbs[,c("decade","key","mode","explicit")] <-lapply(dbs[,c("decade","key","mode","explicit")],as.factor)

#1) Popolarità ~ Acusticità
mod<-lm(popularity~acousticness,dbs)
summary(mod)

#2) Popolarità ~ Chiave
dbs$key<-mapvalues(db$key,c(0,1,2,3,4,5,6,7,8,9,10,11),c("Do","Do♯","Re","Re♯","Mi","Fa","Fa♯","Sol","Sol♯","La","La♯","Si"))
summary(lm(popularity~key,dbs)) 

#3) Popolarità ~ Decennio
summary(lm(popularity~decade,dbs))

#4) Popolarità ~ Esplicito
summary(lm(popularity~explicit,dbs))

#5) Popolarità ~ Tonalità
summary(lm(popularity~mode,dbs))

###############################################################################

##REGRESSIONE MULTIPLA PER LA PREVISIONE DELLA POPOLARITÀ##

#MODELLO MULTIVARIATO CON LE VARIABILI NUMERICHE
mod<-lm(popularity~acousticness+danceability+duration_ms+energy+instrumentalness+liveness+loudness+speechiness+tempo+valence,dbs)
summary(mod)

#ANALISI DEI RESIDUI
g<-ggplot(augment(mod), aes(x = .fitted, y = .resid)) + geom_point()+xlab(label="Valori stimati")+ylab(label="Residui")
g

#NORMAL QQ-PLOT DEI RESIDUI
res_stand<-rstandard(mod)
qqnorm(res_stand,ylab="Residui standardizzati",xlab="Quantili normali",main="") 
qqline(rstandard(mod))
#I residui appiono omoschedastici e i loro quantili hanno sufficiente corrispondenza con quelli propri della distribuzione Normale.

#CALCOLO DEL VIF
mod<-lm(popularity~acousticness+danceability+energy+instrumentalness+liveness+loudness+speechiness+tempo+valence,dbs)
VIF<-vif(mod)
sqrt(VIF)>2
#Energy risulta affetto da un moderato problema di collinearità

# # # # # # # # # # # # # # # # # # # # # #  
  
#MODELLO DELLE VARIABILI QUANTITATIVE SENZA ENERGY
mod1<-lm(popularity~acousticness+danceability+instrumentalness+liveness+loudness+speechiness+tempo+valence,dbs)
summary(mod1)

# # # # # # # # # # # # # # # # # # # # # #  

#AGGIUNTA DELLE VARIABILI QUALITATIVE
mod2<-lm(popularity~acousticness+danceability+instrumentalness+liveness+loudness+speechiness+tempo+valence+explicit+key+mode+decade,dbs)
summary(mod2)

#ANALISI DEI RESIDUI
g<-ggplot(augment(mod2), aes(x = .fitted, y = .resid)) + geom_point()+xlab(label="Valori stimati")+ylab(label="Residui")
g

#NORMAL QQ-PLOT DEI RESIDUI
res_stand<-rstandard(mod2)
qqnorm(res_stand,ylab="Residui standardizzati",xlab="Quantili normali",main="") 
qqline(rstandard(mod))
#La variabilità dei residui appare disomogenea e la somiglianza fra i quantili empirici e i quantili normali si riduce.

#CALCOLO DEL VIF
VIF<-vif(mod2)
VIF
#Non vi sono più problemi di multicollinearità.

###############################################################################  
  
##REGRESSIONE MULTIPLA PER LA PREVISIONE DEL GRADO DI POSITIVITÀ##

mod4<-lm(valence~key+mode+decade+energy+loudness+tempo,dbs)
summary(mod4)


#CALCOLO DEL VIF
VIF<-vif(mod4)
VIF

